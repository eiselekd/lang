from template import *;

class Header(TemplateEngine):
    templ = """ * {{n}}
    {{args}}""";
    def __init__(self, n, *args):
        super(Header,self).__init__();
        self.d['n'] = n;
        self.d['args'] = list(args);

    def doTemplate(self,style='default'):
        return  self.replaceTemplate(Header.templ,style)

class Section(TemplateEngine):
    templ = """ ** {{n}}
    {{args}}""";
    def __init__(self, n, *args):
        super(Section,self).__init__();
        self.d['n'] = n;
        self.d['args'] = list(args);
    def doTemplate(self,style='default'):
        return  self.replaceTemplate(Section.templ,style)

class Paragraph(TemplateEngine):
    templ = """ <p> {{n}}
    {{args}} </p>""";
    def __init__(self, n, *args):
        super(Paragraph,self).__init__();
        self.d['n'] = n;
        self.d['args'] = list(args);
    def doTemplate(self,style='default'):
        return  self.replaceTemplate(Paragraph.templ,style)

def unit_test1():
    
    c = Header("h1",
           Section("s1",
                   Paragraph("Para1 test"),
                   Paragraph("Para2 test")),
           Section("s2",
                   Paragraph("Para3 test"),
                   Paragraph("Para4 test")));
    
    a = c.doTemplate();
    print a;
    
def unit():
    unit_test1();    

if __name__ == '__main__':
    
    import argparse, sys
    parser = argparse.ArgumentParser()
    parser.add_argument('-u', '--unit', action="count", default=0)
    opts = parser.parse_args(sys.argv[1:])
    
    if opts.unit:
        unit();
    


    
