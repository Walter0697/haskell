#!/bin/bash

compile()
{
	echo "compiling file $1" 
	ghc -o $1 "$1.hs"
	echo "done!"
}

clean()
{
	echo "cleaning file $1"
	rm -f "$1.hi"
	rm -f "$1.o"
	rm -f $1	
	echo "done!"
}

if [ "$1" = "compile" ] 
then
	compile $2
elif [ "$1" = "clean" ] 
then
	clean $2
else
	echo "please select an action : <compile | clean>"
fi
