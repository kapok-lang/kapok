Take the Red Pill
==========

## Installing Kapok

Currently Kapok doesn't have a pre-compiled release, so you need to compile it from source. There are dependencies to be set up before doing the compilation.

### Dependencies

To compile from source in platform Unix/Linux or Mac OS, these packages must be installed first: make, erlang, rebar 2.

#### Mac OS X

Update the homebrew to latest before you use homebrew to install these packages, like:

```shell
$ brew update
$ brew install erlang rebar
```

#### Unix/Linux

* Debian/Ubuntu

    Since `yecc` is in a standalone package `erlang-parsetools`, you need to install it besides the package `erlang`.

    ```shell
    $ sudo aptitude install erlang erlang-parsetools rebar
    ```

* Other distributions

    Check and install erlang(with yecc), and rebar 2 via package manager of this distribution or source code.

### Compiling from source

Download the latest release, unpack it and then run `make` inside the unpacked directory.

Or you could compile from the master branch:

```shell
$ git clone https://github.com/kapok-lang/kapok.git
$ cd kapok
$ make
```

After the compiling is done, you are ready to run the `kapok` and `kapokc` commands from the bin directory. It is recommanded that you [add Kapak's bin path to your PATH environment variable](#setting-path-enviroment-variable) to ease development.

### <a id="setting-path-enviroment-variable">Setting PATH enviroment variable</a>

It is highly recommended to add Kapok's bin path to your PATH enviroment variable to ease development.

On Unix systems, you need to find your shell profile file, and then add to the end of this file the following line reflecting the path to your Kapok installation:

```shell
export PATH="$PATH:/path/to/kapok/bin"
```
