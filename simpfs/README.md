# simpfs (Simple FileSystem)

Simple FileSystem is exactly what it sounds like, a simple tool to create an image loadable and usable as a sort of init RAM disk for raspios. It's not very robust, and the code could have been written much better, but the whole purpose of it is to have a relatively simple and working file system in RAM without much complications.

## Build

Does not require cross-compilation, can be built natively on Linux with a simple `make` command.

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
