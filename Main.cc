#include <Dictionary.hh>
#include <ErlComm.hh>
#include <ei.h>
#include <erl_interface.h>
#include <cstring>
#include <cassert>

static const int BufSize = 1024;

// Communication between Erlang and this program is through
// stdin and stdout. If tracing is needed from this program it
// must be written to stderr or a separate file.
int main() {
  erl_init(NULL, 0);
  ErlComm::Byte buf[BufSize];
  // Receive loop of messages from Erlang
  while (ErlComm::receive(buf) > 0) {

    // Decode the message tuple
    ETERM *tuple = erl_decode(buf);
    assert(ERL_IS_TUPLE(tuple));

    // Pick the first element of the tuple - the function to execute
    ETERM *func = erl_element(1, tuple);
    assert(ERL_IS_ATOM(func));

    if (ErlComm::atomEqualsTo(func, "create")) {
      // StaticDecl "Dictionary" "create" [Ptr (UserDef "Dictionary")]
      Dictionary * ret = Dictionary::create();
      ETERM *ptr =
        erl_mk_ulonglong(reinterpret_cast<unsigned long long>(ret));
      erl_encode(ptr, buf);
      ErlComm::send(buf, erl_term_len(ptr));
      erl_free_term(ptr);
    } else if (ErlComm::atomEqualsTo(func, "size")) {
      // MethodDecl "size" [Ptr (UserDef "Dictionary"),Value Integer]
      Dictionary *obj = ErlComm::ptrFromIntegral<Dictionary *>(erl_element(2, tuple));
      int ret = obj->size();
      ETERM *anInt = erl_mk_int(ret);
      erl_encode(anInt, buf);
      ErlComm::send(buf, erl_term_len(anInt));
      erl_free_term(anInt);
    } else if (ErlComm::atomEqualsTo(func, "reference")) {
      // MethodDecl "reference" [Ptr (UserDef "Dictionary"),Value String,Value Void]
      Dictionary *obj = ErlComm::ptrFromIntegral<Dictionary *>(erl_element(2, tuple));
      obj->reference(ErlComm::fromBinary(erl_element(3, tuple)));
      ETERM *ok = erl_mk_atom("ok");
      erl_encode(ok, buf);
      ErlComm::send(buf, erl_term_len(ok));
      erl_free_term(ok);
    } else if (ErlComm::atomEqualsTo(func, "unreference")) {
      // MethodDecl "unreference" [Ptr (UserDef "Dictionary"),Value String,Value Void]
      Dictionary *obj = ErlComm::ptrFromIntegral<Dictionary *>(erl_element(2, tuple));
      obj->unreference(ErlComm::fromBinary(erl_element(3, tuple)));
      ETERM *ok = erl_mk_atom("ok");
      erl_encode(ok, buf);
      ErlComm::send(buf, erl_term_len(ok));
      erl_free_term(ok);
    } else if (ErlComm::atomEqualsTo(func, "count")) {
      // MethodDecl "count" [Ptr (UserDef "Dictionary"),Value String,Value Integer]
      Dictionary *obj = ErlComm::ptrFromIntegral<Dictionary *>(erl_element(2, tuple));
      int ret = obj->count(ErlComm::fromBinary(erl_element(3, tuple)));
      ETERM *anInt = erl_mk_int(ret);
      erl_encode(anInt, buf);
      ErlComm::send(buf, erl_term_len(anInt));
      erl_free_term(anInt);
    } else  {
      // Unknown message
      assert(false);
    }

    erl_free_compound(tuple);
    erl_free_term(func);
  }
  return 0;
}
