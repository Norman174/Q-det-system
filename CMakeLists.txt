#cmake_minimum_required(VERSION 3.20)
#project(VKR)

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14 -fopenmp")
#set(CMAKE_CXX_STANDARD 14 -fopenmp)

#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++19 -fopenmp")

#set(SOURCE_FILES main.cpp)

#add_executable(VKR main.cpp)

cmake_minimum_required(VERSION 3.9)
project(VKR) # you can change the project name

find_package(OpenMP)

set(SOURCE_FILES main.cpp settings.cp)

add_executable(VKR main.cpp)

if(OpenMP_CXX_FOUND)
    target_link_libraries(VKR PUBLIC OpenMP::OpenMP_CXX)
endif()