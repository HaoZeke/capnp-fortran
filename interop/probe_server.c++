// Cap'n Proto 1.4.0 peer for the Fortran RPC scenarios.
// One Box server is shared by every connection. begin() drops the
// capabilities this process is holding so the next scenario starts clean.
#include "probe.capnp.h"
#include <capnp/ez-rpc.h>
#include <kj/debug.h>
#include <string>

namespace {

struct State {
  int live = 0;
};

class LiveCap final : public Live::Server {
 public:
  explicit LiveCap(State& s) : state(s) { ++state.live; }
  ~LiveCap() noexcept { --state.live; }

 private:
  State& state;
};

class BoxImpl final : public Box::Server {
 public:
  kj::Promise<void> begin(BeginContext) override {
    peer = nullptr;
    shared = nullptr;
    haveShared = false;
    liveFulfiller = nullptr;
    peerFulfiller = nullptr;
    return kj::READY_NOW;
  }

  kj::Promise<void> getPromise(GetPromiseContext ctx) override {
    auto paf = kj::newPromiseAndFulfiller<Live::Client>();
    ctx.getResults().setCap(kj::mv(paf.promise));
    liveFulfiller = kj::mv(paf.fulfiller);
    return kj::READY_NOW;
  }

  kj::Promise<void> resolveNow(ResolveNowContext) override {
    KJ_REQUIRE(liveFulfiller.get() != nullptr, "no live promise");
    liveFulfiller->fulfill(kj::heap<LiveCap>(state));
    liveFulfiller = nullptr;
    return kj::READY_NOW;
  }

  kj::Promise<void> live(LiveContext ctx) override {
    ctx.getResults().setN(state.live);
    return kj::READY_NOW;
  }

  kj::Promise<void> setPeer(SetPeerContext ctx) override {
    peer = ctx.getParams().getEcho();
    return kj::READY_NOW;
  }

  kj::Promise<void> tailEcho(TailEchoContext ctx) override {
    auto req = peer.echoRequest();
    req.setN(ctx.getParams().getN());
    return ctx.tailCall(kj::mv(req));
  }

  kj::Promise<void> tailFail(TailFailContext ctx) override {
    return ctx.tailCall(peer.failMeRequest());
  }

  kj::Promise<void> sameRemote(SameRemoteContext ctx) override {
    if (!haveShared) {
      shared = kj::heap<LiveCap>(state);
      haveShared = true;
    }
    ctx.getResults().setA(shared);
    ctx.getResults().setB(shared);
    // The result caps are the references under test. Keeping `shared`
    // would leave the object alive after both of them are released.
    shared = nullptr;
    haveShared = false;
    return kj::READY_NOW;
  }

  kj::Promise<void> oneRemote(OneRemoteContext ctx) override {
    ctx.getResults().setCap(kj::heap<LiveCap>(state));
    return kj::READY_NOW;
  }

  kj::Promise<void> promiseOfPeer(PromiseOfPeerContext ctx) override {
    auto paf = kj::newPromiseAndFulfiller<Echo::Client>();
    ctx.getResults().setCap(kj::mv(paf.promise));
    peerFulfiller = kj::mv(paf.fulfiller);
    return kj::READY_NOW;
  }

  kj::Promise<void> resolvePeer(ResolvePeerContext ctx) override {
    KJ_REQUIRE(peerFulfiller.get() != nullptr, "no peer promise");
    {
      Echo::Client copy = peer;
      peerFulfiller->fulfill(kj::mv(copy));
    }
    peerFulfiller = nullptr;
    if (ctx.getParams().getDropRef()) peer = nullptr;
    return kj::READY_NOW;
  }

  kj::Promise<void> tailWorker(TailWorkerContext ctx) override {
    return ctx.tailCall(peer.getWorkerRequest());
  }

 private:
  State state;
  Echo::Client peer = nullptr;
  Live::Client shared = nullptr;
  bool haveShared = false;
  kj::Own<kj::PromiseFulfiller<Live::Client>> liveFulfiller;
  kj::Own<kj::PromiseFulfiller<Echo::Client>> peerFulfiller;
};

}  // namespace

int main(int argc, char** argv) {
  std::string port = argc > 1 ? argv[1] : "43118";
  capnp::EzRpcServer server(kj::heap<BoxImpl>(), "127.0.0.1:" + port);
  auto& waitScope = server.getWaitScope();
  kj::NEVER_DONE.wait(waitScope);
}
