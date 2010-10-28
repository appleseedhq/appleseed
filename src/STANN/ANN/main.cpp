#include "../common/dpoint.hpp"
#include "../common/nnBase.hpp"
//#include "opt_static_chan_IP.hpp"
#include "ANN_W.hpp"
#include <iostream>


using namespace std;

typedef reviver::dpoint<int, 3> Point;

int main()
{
	Point P[3];
	
	Point a(0,0,0);
	Point b(1,1,1);
	Point c(2,2,2);
	
	Point d(3,3,3);
	
	P[0] = a;
	P[1] = b;
	P[2] = c;
	vector<const Point*> ans;
	
	ans.resize(1);
	
	nnBase<Point> *NN;
	
	NN = new ANN_W<Point>(P, 3);
	NN->ksearch(&d, 1, ans);
	
	cout << *(ans[0]) << endl;
}