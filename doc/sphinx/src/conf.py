import sys, os, re

def get_theme_dir():
    """
    Returns path to directory containing this package's theme.
    
    This is designed to be used when setting the ``html_theme_path``
    option within Sphinx's ``conf.py`` file.
    """
    return os.path.abspath(os.path.join(os.path.dirname(__file__), "../theme"))

def default_sidebars():
    """
    Returns a dictionary mapping for the templates used to render the
    sidebar on the index page and sub-pages.
    """
    return {
        '**': ['localtoc.html', 'relations.html', 'searchbox.html'],
        'index': ['searchbox.html'],
        'search': [],
    }

import sphinx_rtd_theme

extensions = ['sphinx.ext.mathjax']
templates_path = ['_templates']

project = u'lang'

AUTHORS = u"eiselekd"
copyright = u'217, '+AUTHORS
source_suffix = '.rst'
master_doc = 'index'
release = 'X.Y.Z-unknown'
version = 'X.Y'

add_module_names = False
pygments_style = 'sphinx'

    
html_theme = 'langtheme'
html_static_path = ['_static']
html_theme_path = [get_theme_dir(), sphinx_rtd_theme.get_html_theme_path()]
print(html_theme_path);
html_show_sphinx = False
html_show_copyright = False
htmlhelp_basename = 'lang'
html_domain_indices = False

