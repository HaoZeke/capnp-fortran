// Cap'n Proto 1.4.0 peer for the promise-release check.
// getPromise returns a promise that is still unresolved. resolveNow
// fulfills it with a Live server. live() reports how many of those
// servers still exist.
#include "livecap.capnp.h"
#include <capnp/ez-rpc.h>
#include <kj/async.h>
#include <string>

namespace {

struct State {
	int live = 0;
};

class LiveCap final : public Live::Server {
public:
	explicit LiveCap(State &s) : state(s) { ++state.live; }
	~LiveCap() noexcept { --state.live; }

private:
	State &state;
};

class BoxImpl final : public Box::Server {
public:
	kj::Promise<void> getPromise(GetPromiseContext ctx) override {
		auto paf = kj::newPromiseAndFulfiller<Live::Client>();
		ctx.getResults().setCap(kj::mv(paf.promise));
		fulfiller = kj::mv(paf.fulfiller);
		return kj::READY_NOW;
	}

	kj::Promise<void> resolveNow(ResolveNowContext) override {
		fulfiller->fulfill(kj::heap<LiveCap>(state));
		return kj::READY_NOW;
	}

	kj::Promise<void> live(LiveContext ctx) override {
		ctx.getResults().setN(state.live);
		return kj::READY_NOW;
	}

private:
	State state;
	kj::Own<kj::PromiseFulfiller<Live::Client>> fulfiller;
};

}  // namespace

int main(int argc, char **argv) {
	std::string port = argc > 1 ? argv[1] : "43118";
	capnp::EzRpcServer server(kj::heap<BoxImpl>(), "127.0.0.1:" + port);
	auto &waitScope = server.getWaitScope();
	kj::NEVER_DONE.wait(waitScope);
}
