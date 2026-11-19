// Reference RPC peer: capnp-C++ serving Adder over rpc-twoparty on
// 127.0.0.1:<argv[1]>. The Fortran vat (rpc_client.f90) bootstraps it and
// calls add(), proving protocol-level compatibility with the reference
// implementation, not just wire-format byte equality.
#include "adder.capnp.h"
#include <capnp/ez-rpc.h>
#include <kj/async.h>
#include <string>

class AdderImpl final : public Adder::Server {
public:
	kj::Promise<void> add(AddContext ctx) override {
		auto p = ctx.getParams();
		ctx.getResults().setSum(p.getA() + p.getB());
		return kj::READY_NOW;
	}

	kj::Promise<void> waitForever(WaitForeverContext) override {
		return kj::Promise<void>(kj::NEVER_DONE).attach(
			kj::defer([this]() { ++canceledCalls; }));
	}

	kj::Promise<void> cancellationCount(CancellationCountContext ctx) override {
		ctx.getResults().setCount(canceledCalls);
		return kj::READY_NOW;
	}

private:
	uint32_t canceledCalls = 0;
};

int main(int argc, char **argv) {
	std::string port = argc > 1 ? argv[1] : "43117";
	capnp::EzRpcServer server(kj::heap<AdderImpl>(), "127.0.0.1:" + port);
	auto &waitScope = server.getWaitScope();
	kj::NEVER_DONE.wait(waitScope);
}
