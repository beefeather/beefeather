#include "place.h"

Place::Place(string name, string address, Comment* comment):
	name_(name),
	address_(address),
	comment_(comment),
	pos_rank_(0),
	neg_rank_(0),
	lat_(0),
	lon_(0)  { }

void Place::setName(string name) { name_ = name; }
string Place::getName() { return name_; }

void Place::setAddress(string address) { address_ = address; }
string Place::getAddress() { return address_; } 

Comment* Place::getComment(){return comment_; }

void Place::setRank(int positive, int negative) { pos_rank_ = positive; neg_rank_ = negative; }
int Place::getPositiveRank() { return pos_rank_; }
int Place::getNegativeRank() { return neg_rank_; }
	
void Place::setCoordinates(double lat, double lon) { lat_ = lat; lon_ = lon; }
double Place::getLat() { return lat_; }
double Place::getLon() { return lon_; }
