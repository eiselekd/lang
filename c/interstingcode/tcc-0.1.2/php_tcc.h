/*
  +----------------------------------------------------------------------+
  | PHP Version 5                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2007 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Author: Tsukasa Hamano <hamano@klab.org>                             |
  +----------------------------------------------------------------------+
*/

#ifndef PHP_TCC_H
#define PHP_TCC_H

#include <libtcc.h>

#define PHP_TCC_RES_NAME "TCC State"

extern zend_module_entry tcc_module_entry;
#define phpext_tcc_ptr &tcc_module_entry

#ifdef PHP_WIN32
#define PHP_TCC_API __declspec(dllexport)
#else
#define PHP_TCC_API
#endif

#ifdef ZTS
#include "TSRM.h"
#endif

typedef struct {
	TCCState *state;
	int argc;
	char **argv;
} php_tcc;

PHP_MINIT_FUNCTION(tcc);
PHP_MSHUTDOWN_FUNCTION(tcc);
PHP_RINIT_FUNCTION(tcc);
PHP_RSHUTDOWN_FUNCTION(tcc);
PHP_MINFO_FUNCTION(tcc);

PHP_FUNCTION(tcc_new);
PHP_FUNCTION(tcc_delete);
PHP_FUNCTION(tcc_add_include_path);
PHP_FUNCTION(tcc_add_sysinclude_path);
PHP_FUNCTION(tcc_define_symbol);
PHP_FUNCTION(tcc_undefine_symbol);
PHP_FUNCTION(tcc_add_file);
PHP_FUNCTION(tcc_compile_string);
PHP_FUNCTION(tcc_set_output_type);
PHP_FUNCTION(tcc_output_file);
PHP_FUNCTION(tcc_run);

#ifdef ZTS
#define TCC_G(v) TSRMG(tcc_globals_id, zend_tcc_globals *, v)
#else
#define TCC_G(v) (tcc_globals.v)
#endif

#endif	/* PHP_TCC_H */


/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 * vim600: noet sw=4 ts=4 fdm=marker
 * vim<600: noet sw=4 ts=4
 */
