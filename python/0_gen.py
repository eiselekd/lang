def noyield():
	a = range(20);
	for i in a:
		return(i);

def withyield():
	a = range(20);
	for i in a:
                yield(i);

c = noyield();
b = withyield();
for i in b:
	print(i);
