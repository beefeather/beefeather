#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <string>
#include <vector>

#include "comment.h"
#include "place.h"

#include "commentsLoader.h"

#include "htmlWriter.h"
#include "kmlWriter.h"
#include "geocoder.h"
#include "network.h"
#include "commentsProcessor.h"

using namespace std;

//выведет на экран красиво отформатированный список всех комментариев
void printAllSubcomments(Container* container, int includeLevel){
	vector<Comment*> subComments = container->getSubComments();
	for(vector<Comment*>::iterator it = subComments.begin(); it != subComments.end(); ++it) {
                Comment* rootComment = *it; 
                for(int i = 0; i!=includeLevel; ++i)
                        cout << '-';

                cout << rootComment->getText() << "\n";
		printAllSubcomments(rootComment, includeLevel + 1);
        }
}

int main(int argc, char *argv[])
{	
	if(argc != 3){
		cerr << " Usage: cityPlacesParser lj_URL CityName";
		return 1;
	}
	
	//examples:	
	//string url = "http://tema.livejournal.com/704158.html";
	//string cityName = "Ереван";


	//string url = "http://tema.livejournal.com/685479.html";
	//string cityName = "London";

	//string url = "http://tema.livejournal.com/570773.html";
	//string cityName = "Санкт-Петербург";

	string url(argv[1]);
	string cityName(argv[2]);

	cout << url <<"\n";
	cout << cityName <<"\n";

	//download post
	CommentsLoader commentsLoader;
	LjPost rootComment;
	rootComment.setCity(cityName);

	commentsLoader.LoadComments(url,&rootComment);
	printAllSubcomments(&rootComment, 1);
		
	//find places
	vector<Place> places;

	CommentsProcessor ljProcessor;
	ljProcessor.run("processLjComment.js", &rootComment, &places);    
	
	//save result
	HTMLWriter htmlWriter(places);
	htmlWriter.Save("result.html");	

	KMLWriter kmlWriter(places);
	kmlWriter.Save("result.kml");

	return 0;
}
