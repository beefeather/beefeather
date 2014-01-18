#ifndef H_COMMENT
#define H_COMMENT

#include <string>
#include <vector>

using namespace std;
struct Comment;

/**
* Базовый класс - контейнер, содержащий подэлементы
*/
struct Container {
  vector<Comment*> getSubComments();
  void addSubComment(Comment* subComment);
  virtual int getID() = 0;
  virtual Container* getParent() = 0;
  ~Container();
 private:
  vector<Comment*> subComments_;
};

/**
* Класс поста - используется в качестве rootComment, т.е. корень всех-всех комментариев поста. Также содержит название города.
*/
class LjPost : public Container {
public:
	int getID();
	string getCity();
	void setCity(string city);
	Container* getParent();
private: 
	string city_;
};

/**
* Класс комментария - содержит исследуемый текст, url комментария.
*/
struct Comment : public Container
{
public:
	Comment(string text = "", int id = 0);		
	string getURL();
	void setURL(string url);
	string getText();
	int getID();
	Container* getParent();
    void setParent(Container* parent);
private:
	string text_;
	string url_;
	int id_;
	Container* parent_;

public:
};

#endif
