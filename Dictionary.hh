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

#ifndef DICTIONARY_HH
#define DICTIONARY_HH

#include <map>
#include <string>

// The dictionary class will hold a 'database' over words/strings and
// their reference counts. The class is mostly a simple wrapper around
// std::map, but still hides (at least) two bugs which easily can be
// missed when using traditional static testing strategies
class Dictionary {
public:
  Dictionary() :
    mDictMap() {
  }
  ~Dictionary() {
  }

  // Fetch the number of words registered in the dictionary
  int size() const {
    return mDictMap.size();
  }

  // Make a reference of a word. Create entry in dictionary if needed
  // and increase reference count
  void reference(const std::string& word) {
    DictMap::iterator itr = mDictMap.find(word);
    if (itr == mDictMap.end()) {
      mDictMap.insert(DictMap::value_type(word, 1));
    } else {
#ifdef BUG1
      // Misinterpretation of C++ map::insert to think that insert
      //will overwrite the current entry
      mDictMap.insert(DictMap::value_type(word, itr->second + 1));
#else
      ++itr->second;
#endif
    }
  }

  // Count down the reference count for a word in the
  // dictionary. Remove word from the dictionary if the count reaches
  // zero
  void unreference(const std::string& word) {
    DictMap::iterator itr = mDictMap.find(word);
    if (itr != mDictMap.end()) {
#ifdef BUG2
      // Reference count bug
      --itr->second;
#else
      if (--itr->second == 0) {
	mDictMap.erase(itr);
      }
#endif
    }
  }

  // Fetch the reference count for a word
  int count(const std::string& word) const {
    DictMap::const_iterator itr = mDictMap.find(word);
    return itr != mDictMap.end() ? itr->second : 0;
  }
private:
  Dictionary(const Dictionary&);
  Dictionary operator = (const Dictionary&);

  typedef std::map<std::string, int> DictMap;

  DictMap mDictMap;
};

#endif
