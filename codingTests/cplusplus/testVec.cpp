
#include <iostream>
#include <boost/container/vector.hpp>
#include <boost/timer/timer.hpp>
#include <vector>

using std::vector;

int main(){
  int N = 10000000;
/*  
  std::cout << "This is vector insert time" << std::endl;

  {   
    boost::timer::auto_cpu_timer t; 
    std::vector<float> v;
    for (int i = 0; i < N; ++i)
    {
        v.insert(v.begin(), float(i));
    }
  }
*/

  std::cout << "This is vector push_back time" << std::endl;

  {   
    boost::timer::auto_cpu_timer t; 
    std::vector<float> v;
    for (int i = 0; i < N; ++i)
    {
        v.push_back(float(i));
    }
  }

/*
  std::cout << "This is boost::container::vector insert time" << std::endl;
  {
    boost::timer::auto_cpu_timer t;
    boost::container::vector<float> v;
    for (int i = 0; i < N; ++i)
    {
        v.insert(v.begin(), float(i));
    }
  }
*/
  std::cout << "This is boost::container::vector push_back time" << std::endl;
  {
    boost::timer::auto_cpu_timer t;
    boost::container::vector<float> v;
    for (int i = 0; i < N; ++i)
    {
        v.push_back(float(i));
    }
  }



/*
	int howmany, sum;
	while(true){
		std::cout << "\nHow many odd integers do you want to add? ";
		std::cin >> howmany;
		if(howmany > 0 && howmany <= 1000)
			break;
		else
			std::cout << "\nPlease enter number from 1 to 1000 only"<<std::endl;
	}
	sum = howmany*howmany;
	std::cout << "The sum of the first " << howmany << " odd integers is " << sum << std::endl;
*/

	return 0;
}


