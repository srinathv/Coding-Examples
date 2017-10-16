cmake_minimum_required (VERSION 2.6)
project (Tutorial)
add_executable(Tutorial tutorial.cxx)

set (testVar "/usr/lib64/par/foo")
set (badVar "/par/foo/usr/lib")
set (badDir "/usr/lib")

message(STATUS "let's see what is test var = ${testVar}")

if (${testVar} MATCHES "^${badDir}")
  message (STATUS "testVar I think we know regex")
else(${testVar} MATCHES "^${badDir}")
  message (STATUS "testVar I don't know how to regex")
endif (${testVar} MATCHES "^${badDir}")

message(STATUS,"The system is ${CMAKE_SYSTEM_NAME}")

if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  set (MACOSX YES)
    add_definitions(-DMACOSX)
endif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
if (MACOSX)
  message(STATUS," On a mac os X")
endif(MACOSX)


# if (${badVar} MATCHES "^${badDir}")
#    message (STATUS "badVar I think we know regex")
# else(${badVar} MATCHES "^${badDir}")
#    message (STATIS "badVar I don't know how to regex")
# endif (${barVar} MATCHES "^${badDir}")
