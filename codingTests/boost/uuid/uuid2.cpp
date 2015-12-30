#include <iostream>

#include <boost/uuid/uuid.hpp>            // uuid class
#include <boost/uuid/uuid_generators.hpp> // generators
#include <boost/uuid/uuid_io.hpp>         // streaming operators etc.


int main()
{
    boost::uuids::random_generator generator;

    boost::uuids::uuid uuid1 = generator();
    std::cout << uuid1 << std::endl;

    boost::uuids::uuid uuid2 = generator();
    std::cout << uuid2 << std::endl;

    return 0;
}
