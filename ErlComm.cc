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

#include "ErlComm.hh"
#include <cstring>

ssize_t
ErlComm::receive(Byte *buf) {
  assert(buf != NULL);
  if (readBytes(buf, 2) != 2) {
    return -1;
  }
  ssize_t len = (buf[0] << 8 | buf[1]);
  return readBytes(buf, len);
}

ssize_t
ErlComm::send(Byte *buf, std::size_t len) {
  assert(buf != NULL);
  Byte lenByte1 = (len >> 8) & 0xff;
  Byte lenByte2 = len & 0xff;
  writeBytes(&lenByte1, 1);
  writeBytes(&lenByte2, 1);
  return writeBytes(buf, len);
}

bool
ErlComm::atomEqualsTo(const ETERM *atom, const char *str) {
  assert(ERL_IS_ATOM(atom));
  return std::strncmp(ERL_ATOM_PTR(atom), str, ERL_ATOM_SIZE(atom)) == 0;
}

std::string
ErlComm::fromBinary(ETERM *binary) {
  assert(ERL_IS_BINARY(binary));
  // This probably brakes compiler optimization though ...
  std::string str = 
    std::string(reinterpret_cast<const char*>(ERL_BIN_PTR(binary)), 
		static_cast<std::size_t>(ERL_BIN_SIZE(binary)));
  erl_free_term(binary);
  return str;
}

ssize_t
ErlComm::readBytes(Byte *buf, ssize_t len) {
  assert(len >= 0);
  for (ssize_t n, got = 0; got < len; got += n) {
    if ((n = ::read(0, buf+got, len-got)) <= 0) {
      return n;
    }
  }
  return len;
}

ssize_t
ErlComm::writeBytes(const Byte *const buf, ssize_t len) {
  assert(len >= 0);
  for (ssize_t n, wrote = 0; wrote < len; wrote += n) {
    if ((n = ::write(1, buf+wrote, len-wrote)) <= 0) {
      return n;
    }
  }
  return len;
}

