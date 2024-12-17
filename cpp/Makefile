all:
	cd .. && cargo build --release
	$(CXX) -g -std=c++20 test.cpp -I../target/release/ -L../target/release/ -lllguidance_bg -o test
	./test