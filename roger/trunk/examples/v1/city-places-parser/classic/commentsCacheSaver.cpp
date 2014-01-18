#include "commentsCacheSaver.h"

//сохраняет себя
void CommentsCacheSaver::SaveSubcomment(ofstream& file, Comment* rootComment, int includeLevel){
	for(int i = 0; i!=includeLevel; ++i)
		file << '-';

	file << "|" << rootComment->getID() << "|" << rootComment->getText() << "\n";
    SaveContainer(file, rootComment, includeLevel);
}

//сохраняет все свои подкомментарии
void CommentsCacheSaver::SaveContainer(ofstream& file, Container* rootComment, int includeLevel) {
	vector<Comment*> subComments = rootComment->getSubComments();
	for(vector<Comment*>::iterator it = subComments.begin(); it != subComments.end(); ++it)
		SaveSubcomment(file, *it, includeLevel + 1);
}

void CommentsCacheSaver::SaveComments(string path, LjPost* rootComment){
	filePath_ = path;
	rootComment_ = rootComment;
   	ofstream fout;
    fout.open(filePath_.c_str());
	fout << "|-1|\n"; //корень дерева комментариев
    SaveContainer(fout, rootComment_, 0);
	fout.close();  
}
