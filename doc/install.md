Install
==========

## Compiling from source

Currently Kapok doesn't have a pre-compiled release, so you need to compile it from source. To compile from source in platform Unix/Linux or Mac OS, these packages must be installed first: Erlang, Rebar.

Next you should download the latest release, unpack it and then run `make` inside the unpacked directory.

Or you could also compile from the master branch:

```shell
$ git clone https://github.com/kapok-lang/kapok.git
$ cd kapok
$ make
```

After the compiling is done, you are ready to run the `kapok` and `kapokc` commands from the bin directory. It is recommanded that you [add Kapak's bin path to your PATH environment variable](#setting-path-enviroment-variable) to ease development.

## <a id="setting-path-enviroment-variable">Setting PATH enviroment variable</a>

It is highly recommended to add Kapok's bin path to your PATH enviroment variable to ease development.

On Unix systems, you need to find your shell profile file, and then add to the end of this file the following line reflecting the path to your Kapok installation:

```shell
export PATH="$PATH:/path/to/kapok/bin"
```
