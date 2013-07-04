Introduction
============

The proper-play package is a small demo in how to test a C++ class
from Erlang by using the property testing library PropEr (see
http://proper.softlab.ntua.gr/).

NB: Parts of the code is generated, and maybe not that beautiful

Configuration
-------------

* Ensure that Erlang/OTP is installed
* Ensure that the GNU C++ tool chain is installed
* Install PropEr (use the link above, or directly from
  https://github.com/manopapad/proper)
* Include the PropEr installation path in the ERL_LIBS environment
  variable
* Setup the ERL_INTERFACE environment variable to point to your
  erl_interface installation, e.g.:
  /usr/lib/erlang/lib/erl_interface-3.7.11/

Building
--------

The ordinary build cycle is:

* make - to make an ordinary build without the "planted" bugs in the
  C++ class
* make clean - clean the working directory
* make test - run the PropEr tests

Injecting C++ bugs
------------------

There are two "planted" bugs in the C++ class which can be activated
when re-building after a clean. If using Bash the bugs are activated
using:

* BUGS=1 make - Activate "bug 1"
* BUGS=2 make - Activate "bug 2"
* BUGS=3 make - Activate both bugs

Using code generation of glue code
----------------------------------

It it possible to generate the C++ and Erlang glue code yourselves. To
do so clone and build the proper-play-gen toy compiler:
https://github.com/SneakingCat/proper-play-gen

Then proper-play-gen -f Dictionary.mod
