// Copyright (C) 2013  Patrik Sandahl
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "Dictionary.hh"
#include "ErlComm.hh"
#include <ei.h>
#include <erl_interface.h>
#include <iostream>
#include <cstring>
#include <cassert>

static const int BufSize      = 1024;

// The communication token from Erlang is a tuple of the format:
// {<atom specifying function> [, <integral specifying 'this'>, [, Argument]*]}
// Example:
// {new} - create a new instance
// {size, Object} - request the size method
// {reference, Object, Word} - request the reference method
static const int Func         = 1;
static const int Object       = 2;
static const int FirstUserArg = 3;

// Communication between Erlang and the system under test is through
// stdin and stdout. If tracing is needed from this program is has to
// be written to stderr or a separate file
int main() {
  erl_init(NULL, 0);
  ErlComm::Byte buf[BufSize];

  // Receive loop of messages from Erlang
  while (ErlComm::receive(buf) > 0) {

    // Always decode the message tuple
    ETERM *tuple = erl_decode(buf);
    assert(ERL_IS_TUPLE(tuple));

    // Pick the first element of the tuple. It shall be an atom
    // specifying the function to be called
    ETERM *func  = erl_element(Func, tuple);
    assert(ERL_IS_ATOM(func));

    if (ErlComm::atomEqualsTo(func, "create")) {
      // Static function 'create' is requested. No arguments
      Dictionary *dictionary = Dictionary::create();

      ETERM *object = 
	erl_mk_ulonglong(reinterpret_cast<unsigned long long>(dictionary));
      erl_encode(object, buf);
      ErlComm::send(buf, erl_term_len(object));

      erl_free_term(object);
    } else if (ErlComm::atomEqualsTo(func, "size")) {
      // Function 'size' is requested. Object pointer is the only argument
      Dictionary *dictionary = 
	ErlComm::ptrFromIntegral<Dictionary*>(erl_element(Object, tuple));

      ETERM *size = erl_mk_int(dictionary->size());
      erl_encode(size, buf);
      ErlComm::send(buf, erl_term_len(size));

      erl_free_term(size);
    } else if (ErlComm::atomEqualsTo(func, "reference")) {
      // Function 'reference' is requested. Object pointer and a
      // string (transported as a binary) are the arguments
      Dictionary *dictionary = 
	ErlComm::ptrFromIntegral<Dictionary*>(erl_element(Object, tuple));

      dictionary->
	reference(ErlComm::fromBinary(erl_element(FirstUserArg, tuple))
		  );
      ETERM *ok = erl_mk_atom("ok");
      erl_encode(ok, buf);
      ErlComm::send(buf, erl_term_len(ok));

      erl_free_term(ok);
    } else if (ErlComm::atomEqualsTo(func, "unreference")) {
      // Function 'unreference' is requested. Object pointer and a
      // string (transported as a binary) are the arguments
      Dictionary *dictionary =
	ErlComm::ptrFromIntegral<Dictionary*>(erl_element(Object, tuple));

      dictionary->
	unreference(ErlComm::fromBinary(erl_element(FirstUserArg, tuple))
		    );
      ETERM *ok = erl_mk_atom("ok");
      erl_encode(ok, buf);
      ErlComm::send(buf, erl_term_len(ok));

      erl_free_term(ok);
    } else if (ErlComm::atomEqualsTo(func, "count")) {
      // Function 'count' is requested. Oject pointer and a string
      // (transported as a binary) are the arguments
      Dictionary *dictionary = 
	ErlComm::ptrFromIntegral<Dictionary*>(erl_element(Object, tuple));

      ETERM *count = 
	erl_mk_int(dictionary->
		   count(ErlComm::fromBinary(erl_element(FirstUserArg, tuple)))
		   );
      erl_encode(count, buf);
      ErlComm::send(buf, erl_term_len(count));

      erl_free_term(count);
    } else {
      // Unknown function
      assert(false);
    }

    erl_free_compound(tuple);
    erl_free_term(func);
  }

  return 0;
}
