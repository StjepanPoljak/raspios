# simpfs (Simple FileSystem)

Simple FileSystem is exactly what it sounds like, a simple tool to create an image loadable and usable as a sort of init RAM disk for raspios. It's written in Haskell, so you will need GHC to compile.

## Build

First, make sure GHC (Haskell compiler) is installed. Then, run the build with a simple `make` command.

## Usage

### Create image

To create an image:

```shell
./simpfs <folder> -a <file_name>
```

If `<folder>` is not specified, it will simply use the current working directory to make an image.

### Print contents

To print contents of the image in form of a tree:

```shell
./simpfs <image> -l
```

### Inspect node

You can get contents of file or directory in the image by using:

```shell
./simpfs <image> -i <path/to/node>
```

In case of failure, the program will return `-EINVAL`. If the directory or file is found in the provided path, it will print out its contents.
