#!/bin/bash
cat ../test_files/param2
time ./main.native ../test_files/param2 ../test_files/input2 
cat ../test_files/param1
time ./main.native ../test_files/param1 ../test_files/input2
cat ../test_files/param3
time ./main.native ../test_files/param3 ../test_files/input2
cat ../test_files/param4
time ./main.native ../test_files/param4 ../test_files/input2
cat ../test_files/param5
time ./main.native ../test_files/param5 ../test_files/input2 
