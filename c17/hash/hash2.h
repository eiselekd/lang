#ifndef DEF_HASH1_H
#define DEF_HASH1_H
#include <vector>
#include <forward_list>
#include <string>
#include "./expose.h"
using namespace std;

struct HashNode {
    HashNode(string k, int v) : k(k), v(v) { };
    string k;
    int v;
};

class CHash
{
public:
    CHash() {
	bucketcnt = 16;
	buckets.resize(16);
    }

    int push(string k, int v) {
	int h = 0;
	HashNode v0(k,v);
	auto &b = buckets[h];
	for (auto& i : proxy(b)) {
	    if ((*i).k == k) {
		printf("Multimap\n");
		b.insert(i, std::move(v0));
		return 0;
	    }
	}
	b.push_front(std::move(v0));
    }
    vector<list<HashNode>> buckets;
    int bucketcnt;
};



#endif
