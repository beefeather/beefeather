#include "commentsLoader.h"


CommentsLoader::CommentsLoader() {	
	filePathPrefix_ = "saved_"; //устанавливаем префикс перед всеми файлами кэша
}	

void CommentsLoader::LoadComments(string url, LjPost* rootComment) {	
	url_ = url;	
	rootComment_ = rootComment;

	//Выдираем номер поста из url
	//example url: http://tema.livejournal.com/704158.html?page=1comments
	int postNumBeginPos = url.find(".com/");	
	if(postNumBeginPos == -1) return;
	postNumBeginPos += 5;
	int postNumEndPos = url.find(".html", postNumBeginPos);	
	postNum_ = url.substr(postNumBeginPos, postNumEndPos - postNumBeginPos);
	filePath_ = filePathPrefix_ + postNum_;	

	//пытаемся открыть кэш, в зависимости от успеха/неудачи грузим либо из кэша, либо из сети.
	ifstream input_file;
   	input_file.open(filePath_.c_str());	
	if (input_file.fail()){
		cout << "No cached found. Loading from web...\n";

		CommentsWebLoader loader;
		loader.LoadComments(url_, rootComment_, filePathPrefix_);

		CommentsCacheSaver saver;
		saver.SaveComments(filePath_, rootComment_);
	}
	else{		
		cout << "Loading from cache...\n";
		input_file.close();

		CommentsCacheLoader loader;
		loader.LoadCommentsFromCache(url_, filePath_, rootComment_);
	}				
}
