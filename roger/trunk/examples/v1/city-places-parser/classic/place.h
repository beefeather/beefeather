#ifndef H_PLACE
#define H_PLACE

#include <string>
#include <vector>
#include "comment.h"

using namespace std;

/**
* Место, элемент результата программы
*/
struct Place
{
public:
	Place(string name, string adress, Comment* comment);	

	//название места
	void setName(string name);
	string getName();

	//его адресс
	void setAddress(string adress);
	string getAddress();

	//положительная/негативная оценки (независимы друг от друга)
	void setRank(int positive, int negative);
	int getPositiveRank();
	int getNegativeRank();
	
	//комментарий, в котором было найдено это место
	Comment* getComment();

	//координаты
	void setCoordinates(double lat, double lon);
	double getLat();
	double getLon();

private:
	string name_;
	string address_;
	Comment* comment_;
	int pos_rank_;
	int neg_rank_;
	double lon_;
	double lat_;
};

#endif
