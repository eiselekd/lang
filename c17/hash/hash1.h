#ifndef DEF_HASH1_H
#define DEF_HASH1_H
#include <vector>
#include <forward_list>
#include <string>
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
	for (auto& i : buckets[h]) {
	    if (i.k == k) {
		i.v = v;
		return 0;
	    }
	}
	buckets[h].push_front(HashNode(k,v));
    }
    vector<forward_list<HashNode>> buckets;
    int bucketcnt;
};



#endif
