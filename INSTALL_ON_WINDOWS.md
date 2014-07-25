When using the `Setup.hs` script from the *configuration-tools* package
with a source build from a git repository a git binary must be available
in the `PATH` environment of the installation command. This is, for instance,
the case for the test-suite of the *configuration-tools* package itself.

The first step in the following description installs a git binary that
is globally available in the system. Steps two through seven explain
how to setup a Haskell build environment on a Windows system in general.
Those steps are not specific to the *configuration-tools* package. Steps
eight and nine explain how to install and test the *configuration-tools*
package.

Windows
=======

The following steps make use of the `Git Bash` terminal. Beside of being
convenient for users that are familiar with POSIX systems, it is also a
requirement for installing the *network* package, which is a dependency of
*cabal*. Most other Haskell packages can be installed from a windows shell
(PowerShell or cmd shell) as well.

1.  Install Git from: [Git for windows](http://git-scm.com/download/win)

    During the installation, when ask about the environment settings,
    select the second option that makes the git command available at
    the windows shell prompt. Otherwise the git binary is available
    only when `cabal` is executed from the *Git Bash* terminal.

2.  Download the
    [GHC binary distribution for windows](http://www.haskell.org/ghc/dist/7.8.3/ghc-7.8.3-x86_64-unknown-mingw32.tar.bz2)
    and unpack it in a convenient place.

    In the following we assume that it was unpacked in `C:\` into the default directory `C:\ghc-7.8.3`.

3.  Obtain a windows cabal binary, for example from the Haskell Platform distribution
    or a previous GHC installation (the version doesn't matter).

4.  Open the *Git Bash* terminal. A link to it should have been created in the start menu
    during the Git installation.

5.  Include the `bin` directory and `mingw/bin` directory from the GHC binary distribution into
    the `PATH` environment:

    ~~~{.sh}
    export PATH=/c/ghc-7.8.3/bin:/c/ghc-7.8.3/mingw/bin:$PATH
    ~~~

6.  Make sure a cabal binary is available in the `PATH`

    ~~~{.sh}
    which cabal
    ~~~

    and check that `ghc`, `ld`, and `gcc` are used from the respective directories under 
    `/c/ghc-7.8.3/*`:

    ~~~{.sh}
    which ghc
    which ld
    which gcc
    ~~~

7.  Install an updated version of `cabal`:

    ~~~{.sh}
    cabal update
    cabal install cabal-install
    ~~~

    Make sure that the location into which the resulting cabal binary is installed
    (the location is configured in `~/AppData/Roaming/cabal/config` and it is also
    printed to the installation logs) is in your `PATH`.

    Check that you are now using the most recent version of cabal:

    ~~~{.sh}
    which cabal
    cabal --version
    ~~~

8.  Install the *configuration-tools* package from Hackage:

    ~~~{.sh}
    cabal install configuration-tools --enable-tests
    ~~~

    Alternativly, you my clone and install it from
    [GitHub](https://github.com/alephcloud/hs-configuration-tools.git):

    ~~~{.sh}
    git clone https://github.com/alephcloud/hs-configuration-tools.git
    cd hs-configuration-tools
    cabal install --enable-tests
    ~~~

9.  When you installed *configuration-tools* from source you may run the tests
    from within the package directory as follows:

    ~~~{.sh}
    cabal test
    ~~~

    You can run the test with different command line options using cabals `--test-option`
    flag. For instance[^1]:

    ~~~{.sh}
    cabal test --show-details=always --test-option=--help
    cabal test --show-details=always --test-option=--long-info
    ~~~

    [^1]: Note: the windows terminal reacts ungracefully to Unicode characters. In order to have
        it at least not crash the program you should run the following command before you
        exectute any command that may print unicode characters (like the copyright symbol in the
        test examples):

        ~~~{.sh}
        chcp.com 65001
        ~~~
