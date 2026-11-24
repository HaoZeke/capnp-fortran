// Cap'n Proto C++ RpcSystem peer for the RPC differential checker.
// Usage: peer_cpp <port>
// Accepts one connection, serves Target as its bootstrap, and asks the
// driver for its Callback bootstrap at once. Exits when the driver
// disconnects.
#include "diff.capnp.h"
#include <capnp/rpc-twoparty.h>
#include <capnp/rpc.h>
#include <kj/async-io.h>
#include <kj/debug.h>
#include <cstdio>
#include <cstdlib>

namespace {

struct Shared {
  Callback::Client cb = nullptr;
};

class TargetImpl final : public Target::Server {
 public:
  TargetImpl(Shared& sh, int64_t base) : sh(sh), base(base) {}

  kj::Promise<void> echo(EchoContext ctx) override {
    ctx.getResults().setN(ctx.getParams().getN() + base);
    return kj::READY_NOW;
  }

  kj::Promise<void> fail(FailContext) override {
    return KJ_EXCEPTION(FAILED, "SRVEXC");
  }

  kj::Promise<void> make(MakeContext ctx) override {
    ctx.getResults().setCap(kj::heap<TargetImpl>(sh, ctx.getParams().getN()));
    return kj::READY_NOW;
  }

  kj::Promise<void> slow(SlowContext ctx) override {
    auto req = sh.cb.pingRequest();
    req.setN(ctx.getParams().getN());
    return req.send().then([ctx](capnp::Response<N>&& r) mutable {
      ctx.getResults().setN(r.getN());
    });
  }

  kj::Promise<void> slowMake(SlowMakeContext ctx) override {
    auto req = sh.cb.pingRequest();
    req.setN(ctx.getParams().getN());
    return req.send().then([this, ctx](capnp::Response<N>&& r) mutable {
      ctx.getResults().setCap(kj::heap<TargetImpl>(sh, r.getN()));
    });
  }

  kj::Promise<void> self(SelfContext ctx) override {
    ctx.getResults().setCap(thisCap());
    return kj::READY_NOW;
  }

  kj::Promise<void> reflect(ReflectContext ctx) override {
    ctx.getResults().setCap(sh.cb);
    return kj::READY_NOW;
  }

  kj::Promise<void> many(ManyContext ctx) override {
    int64_t n = ctx.getParams().getN();
    if (n < 0) n = 0;
    if (n > 20) n = 20;
    auto ptrs = capnp::AnyStruct::Builder(ctx.getResults()).getPointerSection();
    for (int64_t i = 0; i < n; ++i) {
      ptrs[i].setAs<Target>(kj::heap<TargetImpl>(sh, i));
    }
    return kj::READY_NOW;
  }

 private:
  Shared& sh;
  int64_t base;
};

}  // namespace

int main(int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s <port>\n", argv[0]);
    return 2;
  }
  auto io = kj::setupAsyncIo();
  auto& ws = io.waitScope;
  auto addr = io.provider->getNetwork().parseAddress("127.0.0.1", atoi(argv[1])).wait(ws);
  auto listener = addr->listen();
  printf("READY\n");
  fflush(stdout);
  auto stream = listener->accept().wait(ws);
  capnp::TwoPartyVatNetwork net(*stream, capnp::rpc::twoparty::Side::SERVER);
  Shared sh;
  auto rpc = capnp::makeRpcServer(net, kj::heap<TargetImpl>(sh, 0));
  {
    capnp::MallocMessageBuilder vb;
    auto vid = vb.initRoot<capnp::rpc::twoparty::VatId>();
    vid.setSide(capnp::rpc::twoparty::Side::CLIENT);
    sh.cb = rpc.bootstrap(vid).castAs<Callback>();
  }
  net.onDisconnect().wait(ws);
  sh.cb = nullptr;
  return 0;
}
