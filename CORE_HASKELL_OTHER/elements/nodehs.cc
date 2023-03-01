#include <node.h>
#include <v8.h>
#include <nodehs_stub.h>

using namespace v8;

Handle<Value> wrapHelloWorld(const Arguments& args) {
  HandleScope scope;
  return scope.Close(String::New( (char *)hsHelloWorld() ));
}

void Init(Handle<Object> exports) {
  exports->Set( String::NewSymbol("helloWorld"),
    FunctionTemplate::New(wrapHelloWorld)->GetFunction());
}

NODE_MODULE(nodehs, Init)

