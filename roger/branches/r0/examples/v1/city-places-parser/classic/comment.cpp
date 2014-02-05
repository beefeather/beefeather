#include "comment.h"

Comment::Comment(string text, int id) : text_(text), id_(id), parent_(NULL) { }	

Container::~Container(){
	for(vector<Comment*>::iterator i = subComments_.begin(); i != subComments_.end(); ++i){
		delete *i;		
	}
}

void Container::addSubComment(Comment* subComment){	
	subComment->setParent(this);
	subComments_.push_back(subComment);
}

int LjPost::getID(){
	return -1;
}

Container* LjPost::getParent(){
	return NULL;
}

string LjPost::getCity(){
	return city_;
}	

void LjPost::setCity(string city){
	city_ = city;
}	

string Comment::getText(){
	return text_;
}

int Comment::getID(){
	return id_;
}

void Comment::setURL(string url) {
	url_ = url;
}

string Comment::getURL(){
	return url_;
}

vector<Comment*> Container::getSubComments(){
	return subComments_;
}

Container* Comment::getParent(){
	return parent_;
}

void Comment::setParent(Container* parent){
	parent_ = parent;
}
