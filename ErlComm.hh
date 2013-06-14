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

#ifndef ERLCOMM_HH
#define ERLCOMM_HH

#include <ei.h>
#include <erl_interface.h>
#include <string>
#include <cstddef>
#include <unistd.h>
#include <cassert>

// Helper class to access various aspect of communicating with Erlang
class ErlComm {
public:
  typedef unsigned char Byte;

  // 'Receive' a coded buffer from stdin
  static ssize_t receive(Byte *buf);

  // 'Send' a coded buffer to stdout
  static ssize_t send(Byte *buf, std::size_t len);

  // Compare a string with a provided atom
  static bool atomEqualsTo(const ETERM *atom, const char *str);

  // Extract a C++ string from a provided binary AND release the term
  static std::string fromBinary(ETERM *binary);
  
  // Extract a C++ pointer from a provided Erlang integral AND release
  // the term
  template <class PtrType>
  static PtrType ptrFromIntegral(ETERM *integral) {
    PtrType ptr = NULL;
    
    if (ERL_IS_INTEGER(integral)) {
      ptr = reinterpret_cast<PtrType>(ERL_INT_VALUE(integral));
    } else if (ERL_IS_UNSIGNED_INTEGER(integral)) {
      ptr = reinterpret_cast<PtrType>(ERL_INT_UVALUE(integral));
    } else if (ERL_IS_LONGLONG(integral)) {
      ptr = reinterpret_cast<PtrType>(ERL_LL_VALUE(integral));
    } else if (ERL_IS_UNSIGNED_LONGLONG(integral)) {
      ptr = reinterpret_cast<PtrType>(ERL_LL_UVALUE(integral));
    } else {
      assert(false);
    }

    erl_free_term(integral);
    return ptr;
  }

private:
  ErlComm();
  ErlComm(const ErlComm&);

  static ssize_t readBytes(Byte *buf, ssize_t len);
  static ssize_t writeBytes(const Byte *const buf, ssize_t len);
};

#endif
