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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "php.h"
#include "php_ini.h"
#include "ext/standard/info.h"
#include "php_tcc.h"

/* If you declare any globals in php_tcc.h uncomment this:
ZEND_DECLARE_MODULE_GLOBALS(tcc)
*/

/* True global resources - no need for thread safety here */
static int le_tcc;

/* {{{ tcc_functions[]
 *
 * Every user visible function must have an entry in tcc_functions[].
 */
zend_function_entry tcc_functions[] = {
	PHP_FE(tcc_new, NULL)
	PHP_FE(tcc_delete, NULL)
	PHP_FE(tcc_add_include_path, NULL)
	PHP_FE(tcc_add_sysinclude_path, NULL)
	PHP_FE(tcc_define_symbol, NULL)
	PHP_FE(tcc_undefine_symbol, NULL)
	PHP_FE(tcc_add_file, NULL)
	PHP_FE(tcc_compile_string, NULL)
	PHP_FE(tcc_set_output_type, NULL)
	PHP_FE(tcc_output_file, NULL)
	PHP_FE(tcc_run, NULL)
	{NULL, NULL, NULL}	/* Must be the last line in tcc_functions[] */
};
/* }}} */

/* {{{ tcc_module_entry
 */
zend_module_entry tcc_module_entry = {
#if ZEND_MODULE_API_NO >= 20010901
	STANDARD_MODULE_HEADER,
#endif
	"tcc",
	tcc_functions,
	PHP_MINIT(tcc),
	PHP_MSHUTDOWN(tcc),
	NULL,
	NULL,
	PHP_MINFO(tcc),
#if ZEND_MODULE_API_NO >= 20010901
	"0.1.2", /* Replace with version number for your extension */
#endif
	STANDARD_MODULE_PROPERTIES
};
/* }}} */

#ifdef COMPILE_DL_TCC
ZEND_GET_MODULE(tcc)
#endif

/* {{{ php_tcc_dtor */
static void php_tcc_dtor(zend_rsrc_list_entry *rsrc TSRMLS_DC)
{
	php_tcc *tcc = (php_tcc*)rsrc->ptr;
	if(tcc){
		if(tcc->state){
			tcc_delete(tcc->state);
			tcc->state = NULL;
		}
		if(tcc->argv){
			efree(tcc->argv);
			tcc->argv=NULL;
		}
		efree(tcc);
	}
	return;
}
/* }}} */

/* {{{ php_tcc__error */
void php_tcc_error(void *opaque, const char *msg)
{
	php_printf(msg);
	php_printf("\n");
}
/* }}} */

/* {{{ PHP_MINIT_FUNCTION
 */
PHP_MINIT_FUNCTION(tcc)
{
	/* If you have INI entries, uncomment these lines 
	   REGISTER_INI_ENTRIES();
	*/
	le_tcc = zend_register_list_destructors_ex(php_tcc_dtor, NULL, PHP_TCC_RES_NAME, module_number);

	REGISTER_LONG_CONSTANT("TCC_OUTPUT_MEMORY", TCC_OUTPUT_MEMORY,
						   CONST_CS | CONST_PERSISTENT);
	REGISTER_LONG_CONSTANT("TCC_OUTPUT_EXE", TCC_OUTPUT_EXE,
						   CONST_CS | CONST_PERSISTENT);
	REGISTER_LONG_CONSTANT("TCC_OUTPUT_DLL", TCC_OUTPUT_DLL,
						   CONST_CS | CONST_PERSISTENT);
	REGISTER_LONG_CONSTANT("TCC_OUTPUT_OBJ", TCC_OUTPUT_OBJ,
						   CONST_CS | CONST_PERSISTENT);
	REGISTER_LONG_CONSTANT("TCC_OUTPUT_PREPROCESS", TCC_OUTPUT_PREPROCESS,
						   CONST_CS | CONST_PERSISTENT);

	REGISTER_LONG_CONSTANT("TCC_OUTPUT_FORMAT_ELF", TCC_OUTPUT_FORMAT_ELF,
						   CONST_CS | CONST_PERSISTENT);
	REGISTER_LONG_CONSTANT("TCC_OUTPUT_FORMAT_BINARY",
						   TCC_OUTPUT_FORMAT_BINARY,
						   CONST_CS | CONST_PERSISTENT);
	REGISTER_LONG_CONSTANT("TCC_OUTPUT_FORMAT_COFF", TCC_OUTPUT_FORMAT_COFF,
						   CONST_CS | CONST_PERSISTENT);
	return SUCCESS;
}
/* }}} */

/* {{{ PHP_MSHUTDOWN_FUNCTION
 */
PHP_MSHUTDOWN_FUNCTION(tcc)
{
	return SUCCESS;
}
/* }}} */

/* {{{ PHP_MINFO_FUNCTION
 */
PHP_MINFO_FUNCTION(tcc)
{
	php_info_print_table_start();
	php_info_print_table_header(2, "tcc support", "enabled");
	php_info_print_table_end();
}
/* }}} */

/* {{{ proto resource tcc_new()
   Create a new TCC compilation context */
PHP_FUNCTION(tcc_new)
{
	php_tcc *tcc;
	tcc = emalloc(sizeof(php_tcc));
	tcc->argc = 0;
	tcc->argv = NULL;
	tcc->state = tcc_new();
	if(!tcc->state){
		efree(tcc);
		RETURN_FALSE;
	}
	tcc_set_error_func(tcc->state, NULL, php_tcc_error);
	ZEND_REGISTER_RESOURCE(return_value, tcc, le_tcc);
}
/* }}} */

/* {{{ proto void tcc_delete(resource tcc)
   Free a TCC compilation context */
PHP_FUNCTION(tcc_delete)
{
	zval *ztcc;
	php_tcc *tcc;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC, "r", &ztcc)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	tcc_delete(tcc->state);
	tcc->state = NULL;
	return;
}
/* }}} */

/* {{{ proto int tcc_add_include_path(resource tcc, string pathname)
   Add include path */
PHP_FUNCTION(tcc_add_include_path)
{
	zval *ztcc;
	php_tcc *tcc;
	char *pathname;
	int pathname_len;
	int ret;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rs", &ztcc, &pathname, &pathname_len)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	ret = tcc_add_include_path(tcc->state, pathname);
	RETURN_LONG(ret);
}
/* }}} */

/* {{{ proto int tcc_add_sysinclude_path(resource tcc, string pathname)
   Add in system include path */
PHP_FUNCTION(tcc_add_sysinclude_path)
{
	zval *ztcc;
	php_tcc *tcc;
	char *pathname;
	int pathname_len;
	int ret;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rs", &ztcc, &pathname, &pathname_len)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	ret = tcc_add_sysinclude_path(tcc->state, pathname);
	RETURN_LONG(ret);
}
/* }}} */

/* {{{ proto void tcc_define_symbol(resource tcc, string sym, string value)
   Define preprocessor symbol 'sym'. Can put optional value */
PHP_FUNCTION(tcc_define_symbol)
{
	zval *ztcc;
	php_tcc *tcc;
	char *sym;
	int sym_len;
	char *value;
	int value_len;
	int ret;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rss", &ztcc, &sym, &sym_len, &value, &value_len)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	tcc_define_symbol(tcc->state, sym, value);
	return;
}
/* }}} */

/* {{{ proto void tcc_undefine_symbol(resource tcc, string sym)
   Undefine preprocess symbol 'sym' */
PHP_FUNCTION(tcc_undefine_symbol)
{
	zval *ztcc;
	php_tcc *tcc;
	char *sym;
	int sym_len;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rs", &ztcc, &sym, &sym_len)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	tcc_undefine_symbol(tcc->state, sym);
	return;
}
/* }}} */

/* {{{ proto int tcc_add_file(resource tcc, string filename)
 */
PHP_FUNCTION(tcc_add_file)
{
	zval *ztcc;
	php_tcc *tcc;
	char *filename;
	int filename_len;
	int ret;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rs", &ztcc, &filename, &filename_len)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	ret = tcc_add_file(tcc->state, filename);
	RETURN_LONG(ret);
}
/* }}} */

/* {{{ proto int tcc_compile_string(resource tcc, string buf)
 */
PHP_FUNCTION(tcc_compile_string)
{
	zval *ztcc;
	php_tcc *tcc;
	char *buf;
	int buf_len;
	int ret;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rs", &ztcc, &buf, &buf_len)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	ret = tcc_compile_string(tcc->state, buf);
	RETURN_LONG(ret);
}
/* }}} */

/* {{{ proto int tcc_set_output_type(resource tcc, int output_type)
 */
PHP_FUNCTION(tcc_set_output_type)
{
	zval *ztcc;
	php_tcc *tcc;
	long output_type;
	int ret;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rl", &ztcc, &output_type)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	ret = tcc_set_output_type(tcc->state, output_type);
	RETURN_LONG(ret);
}
/* }}} */

/* {{{ proto int tcc_output_file(resource tcc, int filename)
 */
PHP_FUNCTION(tcc_output_file)
{
	zval *ztcc;
	php_tcc *tcc;
	char *filename;
	int filename_len;
	int ret;

	if (zend_parse_parameters(ZEND_NUM_ARGS() TSRMLS_CC,
							  "rs", &ztcc, &filename, &filename_len)
		== FAILURE) {
		RETURN_FALSE;
	}
	ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1, PHP_TCC_RES_NAME, le_tcc);
	ret = tcc_output_file(tcc->state, filename);
	RETURN_LONG(ret);
}
/* }}} */

/* {{{ proto int tcc_run(resource tcc)
 */
PHP_FUNCTION(tcc_run)
{
	php_tcc *tcc;
	zval *ztcc;
	int ret;
	int i;
	zval *zargv;
	zval **arg;
	HashTable *hash;

	if (zend_parse_parameters_ex(ZEND_PARSE_PARAMS_QUIET,
								 ZEND_NUM_ARGS() TSRMLS_CC, "r", &ztcc)
		== SUCCESS) {
		ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1,
							PHP_TCC_RES_NAME, le_tcc);
		tcc->argc = 0;
		tcc->argv = NULL;
	}else if (zend_parse_parameters_ex(ZEND_PARSE_PARAMS_QUIET,
									   ZEND_NUM_ARGS() TSRMLS_CC,
									   "ra", &ztcc, &zargv)
			  == SUCCESS) {
		ZEND_FETCH_RESOURCE(tcc, php_tcc*, &ztcc, -1,
							PHP_TCC_RES_NAME, le_tcc);
		hash = Z_ARRVAL_P(zargv);
		tcc->argc = zend_hash_num_elements(hash);
		tcc->argv = emalloc(sizeof(char*) * tcc->argc);
		for(i=0; i < tcc->argc; i++){
			if(zend_hash_get_current_data(hash, (void **)&arg) == FAILURE) {
				php_error(E_WARNING,
						  "%s() takes tcc reference",
                          get_active_function_name(TSRMLS_C));
				RETURN_FALSE;
			}
			tcc->argv[i] = Z_STRVAL_PP(arg);
			zend_hash_move_forward(hash);
		}
	}else{
		php_error(E_WARNING,
				  "%s() takes tcc reference",
				  get_active_function_name(TSRMLS_C));
		RETURN_FALSE;
	}
	ret = tcc_run(tcc->state, tcc->argc, tcc->argv);
	RETURN_LONG(ret);
}
/* }}} */

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 * vim600: noet sw=4 ts=4 fdm=marker
 * vim<600: noet sw=4 ts=4
 */
