#!/bin/sh

CCDIR=cc
CURR="`pwd`"

installed() {
	echo -e "(i) Cross-compiler has been installed. You can now run make with:\n\n\tPATH=cc/bin/:\$PATH make\n\nEnjoy."
}

if ! [ -d $CCDIR ]; then
	mkdir $CCDIR
else
	CHECK="`ls $CCDIR/bin/aarch64-none-elf-*`"
	if ! [ -z "$CHECK" ]; then
		installed
		exit 1
	fi
fi

cd $CCDIR

wget "https://developer.arm.com/-/media/Files/downloads/gnu-a/9.2-2019.12/binrel/gcc-arm-9.2-2019.12-x86_64-aarch64-none-elf.tar.xz" -O cc.tar.xz

tar -xf cc.tar.xz

rm -rf cc.tar.xz

CCDIR2="`ls`"

mv "$CCDIR2"/* ./

rmdir "$CCDIR2"

cd "$CURR"

installed

exit 0
