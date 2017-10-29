import re;

class TemplateEngine(object):

    d = {};
    a = {};
    p = None;
    
    def __init__(self,p=None):
        self.d = {}
        self.a = {}
        self.p = p;
        
    def __setitem__(self, k, v):
        self.d[k] = v;
        
    def __getitem__(self, k):
        return self.d[k]
    
    def __iter__(self):
        return iter(self.d);

    def getSubstitution(self,str,a={},style='default'):
        if str in self.d.keys():
            v = self.d[str];
            if isinstance(v,int):
                return ("%d" %(v))
            if isinstance(v,list):
                j = "";
                if 'testn' in a.keys():
                    n = a['testn'];
                    if (n in self.d.keys()) and self.d[n]:
                        return ""
                if 'test' in a.keys():
                    n = a['test'];
                    if (n in self.d.keys()) and not self.d[n]:
                        return ""
                if 'j' in a.keys():
                    j = a['j'];
                pre = ""; post = ""
                if (len(v)):
                    if 'pre' in a.keys():
                        pre = a['pre']
                    if 'post' in a.keys():
                        post = a['post']
                return pre+j.join((self.getOneSubstitution(x,a,style)) for x in v)+post;
            return self.getOneSubstitution(v,a,style);
        raise ValueError();
    
    def getOneSubstitution(self,v,a={},style='default'):
        if isinstance(v,TemplateEngine):
            if 'style' in a.keys():
                style = a['style'];
            return v.doTemplate(style)
        return v;

    # This replaces "{{name}}" section in <str> with str representation of <self>.d[name]
    # Each substitution calls getSubstitution().
    # To spice thing up a parameter hash <args> can be specified "{{name[json]}}" that is passed
    # as a parameter to getSubstitution(args). If <self>.d[[name] is a list args[j] is used as a seperator,
    # args[pre] is prepended and args[post] is appened.
    # It also handles simple {{if ... }} statements
    # If <self>.d[name] itself is TemplateEngine derived , then <self>.d[name].doTemplate() is used to
    # recursively generate the template output.
    def replaceTemplate(self,str,style='default'):
        r = ""; pos = 0; reb =re.compile("(?:\{\{if)(?:\[(.*?)\])(.*?)(?:fi\}\})",re.DOTALL)
        for m in reb.finditer(str):
            r += str[pos:m.start(0)];
            pos = m.end();
            txt = m.group(1);
            b = m.group(2);
            if (txt in self.d.keys()) and self.d[txt]:
                r += b            
        r += str[pos:];
        str = r;
        for i in range(2):
            r = ""; pos = 0; 
            for m in re.finditer("(?:\{\{)(.*?)(?:\[(.*?)\])?(?:\}\})",str):
                a = self.a;
                r += str[pos:m.start(0)];
                pos = m.end();
                txt = m.group(1);
                if m.group(2):
                    b = eval('{' + m.group(2) + '}');
                    a = dict(a.items() + b.items());
                if (len(txt)):
                    try:
                        rtxt = self.getSubstitution(txt,a,style);
                        if 'up' in a.keys():
                            rtxt = rtxt.upper();
                        r += rtxt;
                    except ValueError:
                        r += m.group(0)
                    
            r += str[pos:];
            if not (self.p is None):
                r = self.p.replaceTemplate(r)
            str = r;
        return r;
