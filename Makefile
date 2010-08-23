all:
	cd src; erl -make

clean:
	find ebin -name *.beam -delete
