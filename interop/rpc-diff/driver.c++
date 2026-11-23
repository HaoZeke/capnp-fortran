// Raw-message RPC driver for the differential checker.
// Usage: driver <port> <seed> [steps]
//        driver <port> probe:<NAME>
//
// Connects to a peer that serves diff.capnp Target as its bootstrap and
// plays a script drawn from <seed>. Every choice depends only on the
// seed and on what the peer has said so far, so two peers that answer
// alike receive the same script. The log on stdout names the ids the
// peer picks (question, export and embargo ids) by order of first use,
// so two peers that allocate ids differently still give equal logs.
//
// Log lines:
//   == <n> <action>     a step; lines up to the next marker form a window
//   D> ...              a message the driver sent
//   P> ...              a message the peer sent
// Each step ends with a sync call (echo on the first bootstrap import)
// whose own messages are not logged, then an idle wait.
#include "diff.capnp.h"
#include <capnp/capability.h>
#include <capnp/message.h>
#include <capnp/rpc.capnp.h>
#include <capnp/serialize.h>
#include <kj/async.h>
#include <kj/debug.h>

#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <sys/socket.h>
#include <unistd.h>

#include <algorithm>
#include <chrono>
#include <cstdio>
#include <cstring>
#include <map>
#include <random>
#include <string>
#include <thread>
#include <vector>

namespace rpc = capnp::rpc;

namespace {

const int kSyncTimeoutMs = 4000;
const int kIdleMs = 25;
const int kNoSuchPromise = 9000;

int fd = -1;
std::vector<uint8_t> inbuf;
bool peerEof = false;
bool peerAborted = false;
bool driverAborted = false;
std::mt19937_64 rng;

uint64_t TARGET_ID = capnp::typeId<Target>();
uint64_t CALLBACK_ID = capnp::typeId<Callback>();

// ---------------------------------------------------------------- state

struct Import {  // a peer export the driver holds
  int sym;
  int refs;
  bool promise;
};
std::map<uint32_t, Import> imports;
int nextImportSym = 0;

struct PeerQ {  // a question the peer asked the driver
  int sym;
  bool answered = false;
  bool finished = false;
  std::vector<uint32_t> resultExports;  // driver exports in our Return
};
std::map<uint32_t, PeerQ> peerQs;
int nextPeerQSym = 0;

std::map<uint32_t, int> embargoSyms;
int nextEmbargoSym = 0;

struct Question {  // a question the driver asked
  uint32_t id;
  std::string what;
  bool returned = false;
  bool finished = false;
  bool finishReleased = false;
  bool sync = false;
  std::vector<uint32_t> resultImports;  // senderHosted/senderPromise wires
  std::vector<uint32_t> paramExports;   // driver exports sent in params
};
std::vector<Question> questions;  // index == question id

struct Export {  // a capability the driver hosts
  int refs = 0;
  bool promise = false;
  bool resolved = false;
  bool resolvedToPeer = false;
  uint32_t peerWire = 0;  // receiverHosted target of the resolution
};
std::map<uint32_t, Export> exports;
uint32_t nextExportId = 1;  // 0 is the Callback bootstrap

struct Incoming {  // a call the peer made on a driver capability
  uint32_t wire;
  bool yourself;
  int64_t n;
  uint64_t serial;  // order of arrival; wire ids are reused
};
std::vector<Incoming> pending;
uint64_t incomingCount = 0;

uint32_t bootImportWire = 0;
bool haveBoot = false;
std::vector<std::string> window;
std::vector<std::string> logAll;

// ---------------------------------------------------------------- io

void emit(const std::string& s) {
  window.push_back(s);
}

void flushWindow() {
  for (auto& s : window) {
    printf("%s\n", s.c_str());
  }
  window.clear();
  fflush(stdout);
}

void sendRaw(capnp::MessageBuilder& mb) {
  auto words = capnp::messageToFlatArray(mb);
  auto bytes = words.asBytes();
  size_t off = 0;
  while (off < bytes.size()) {
    ssize_t n = ::send(fd, bytes.begin() + off, bytes.size() - off, MSG_NOSIGNAL);
    if (n <= 0) {
      peerEof = true;
      return;
    }
    off += n;
  }
}

void sendBytes(const std::vector<uint8_t>& bytes) {
  size_t off = 0;
  while (off < bytes.size()) {
    ssize_t n = ::send(fd, bytes.data() + off, bytes.size() - off, MSG_NOSIGNAL);
    if (n <= 0) {
      peerEof = true;
      return;
    }
    off += size_t(n);
  }
}

struct InMsg {
  kj::Array<capnp::word> words;
  kj::Own<capnp::FlatArrayMessageReader> reader;
};

// Returns true when a full message is available in inbuf.
bool parseOne(InMsg& out) {
  if (inbuf.size() < 4) return false;
  uint32_t nseg;
  memcpy(&nseg, inbuf.data(), 4);
  nseg += 1;
  if (nseg > 512) {
    peerEof = true;
    return false;
  }
  size_t hdr = 4 * (1 + nseg);
  hdr = (hdr + 7) & ~size_t(7);
  if (inbuf.size() < hdr) return false;
  size_t total = 0;
  for (uint32_t i = 0; i < nseg; ++i) {
    uint32_t w;
    memcpy(&w, inbuf.data() + 4 * (1 + i), 4);
    total += size_t(w) * 8;
  }
  if (inbuf.size() < hdr + total) return false;
  size_t len = hdr + total;
  out.words = kj::heapArray<capnp::word>(len / 8);
  memcpy(out.words.begin(), inbuf.data(), len);
  inbuf.erase(inbuf.begin(), inbuf.begin() + len);
  capnp::ReaderOptions opts;
  opts.traversalLimitInWords = 64 * 1024 * 1024;
  out.reader = kj::heap<capnp::FlatArrayMessageReader>(out.words, opts);
  return true;
}

// Waits up to timeoutMs for one message. False on timeout or EOF.
bool recvOne(InMsg& out, int timeoutMs) {
  auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(timeoutMs);
  for (;;) {
    if (parseOne(out)) return true;
    if (peerEof) return false;
    auto left = std::chrono::duration_cast<std::chrono::milliseconds>(
                    deadline - std::chrono::steady_clock::now())
                    .count();
    if (left < 0) left = 0;
    pollfd p{fd, POLLIN, 0};
    int r = ::poll(&p, 1, int(left));
    if (r <= 0) return false;
    uint8_t buf[65536];
    ssize_t n = ::recv(fd, buf, sizeof(buf), 0);
    if (n <= 0) {
      peerEof = true;
      return false;
    }
    inbuf.insert(inbuf.end(), buf, buf + n);
  }
}

// ---------------------------------------------------------------- names

std::string importName(uint32_t wire, bool promise) {
  auto it = imports.find(wire);
  if (it == imports.end()) {
    Import im{nextImportSym++, 0, promise};
    it = imports.emplace(wire, im).first;
  }
  it->second.refs += 1;
  it->second.promise = promise;
  return "E" + std::to_string(it->second.sym);
}

std::string importRef(uint32_t wire) {
  auto it = imports.find(wire);
  if (it == imports.end()) return "E?" + std::to_string(wire);
  return "E" + std::to_string(it->second.sym);
}

void dropImportRefs(uint32_t wire, int n) {
  auto it = imports.find(wire);
  if (it == imports.end()) return;
  it->second.refs -= n;
  if (it->second.refs <= 0) imports.erase(it);
}

std::string peerQName(uint32_t wire, bool create) {
  auto it = peerQs.find(wire);
  if (it == peerQs.end()) {
    if (!create) return "P?" + std::to_string(wire);
    PeerQ q;
    q.sym = nextPeerQSym++;
    it = peerQs.emplace(wire, q).first;
  }
  return "P" + std::to_string(it->second.sym);
}

void retirePeerQ(uint32_t wire) {
  auto it = peerQs.find(wire);
  if (it != peerQs.end() && it->second.answered && it->second.finished) peerQs.erase(it);
}

std::string embargoName(uint32_t id) {
  auto it = embargoSyms.find(id);
  if (it == embargoSyms.end()) it = embargoSyms.emplace(id, nextEmbargoSym++).first;
  return "m" + std::to_string(it->second);
}

std::string exceptionStr(rpc::Exception::Reader e) {
  const char* t = "?";
  switch (e.getType()) {
    case rpc::Exception::Type::FAILED: t = "failed"; break;
    case rpc::Exception::Type::OVERLOADED: t = "overloaded"; break;
    case rpc::Exception::Type::DISCONNECTED: t = "disconnected"; break;
    case rpc::Exception::Type::UNIMPLEMENTED: t = "unimplemented"; break;
  }
  return std::string("exc(") + t + ": " + std::string(e.getReason().cStr()) + ")";
}

std::string opsStr(capnp::List<rpc::PromisedAnswer::Op>::Reader ops) {
  std::string s;
  for (auto op : ops) {
    s += ",";
    if (op.isNoop()) {
      s += "noop";
    } else {
      s += std::to_string(op.getGetPointerField());
    }
  }
  return s;
}

// A promisedAnswer the peer sent names a peer question (its own answer
// from the driver) or, inside a capability descriptor, a driver
// question (receiverAnswer from the peer is the driver's question).
std::string capDescPeer(rpc::CapDescriptor::Reader cd, std::vector<uint32_t>* got) {
  switch (cd.which()) {
    case rpc::CapDescriptor::NONE:
      return "none";
    case rpc::CapDescriptor::SENDER_HOSTED:
      if (got) got->push_back(cd.getSenderHosted());
      return "sh:" + importName(cd.getSenderHosted(), false);
    case rpc::CapDescriptor::SENDER_PROMISE:
      if (got) got->push_back(cd.getSenderPromise());
      return "sp:" + importName(cd.getSenderPromise(), true);
    case rpc::CapDescriptor::RECEIVER_HOSTED:
      return "rh:D" + std::to_string(cd.getReceiverHosted());
    case rpc::CapDescriptor::RECEIVER_ANSWER: {
      auto pa = cd.getReceiverAnswer();
      return "ra:(q" + std::to_string(pa.getQuestionId()) + opsStr(pa.getTransform()) + ")";
    }
    case rpc::CapDescriptor::THIRD_PARTY_HOSTED:
      if (got) got->push_back(cd.getThirdPartyHosted().getVineId());
      return "tp:" + importName(cd.getThirdPartyHosted().getVineId(), false);
  }
  return "?";
}

std::string contentStr(capnp::AnyPointer::Reader content) {
  if (content.isNull()) return "null";
  if (!content.isStruct()) return "nonstruct";
  auto s = content.getAs<capnp::AnyStruct>();
  std::string out;
  auto data = s.getDataSection();
  if (data.size() >= 8) {
    int64_t n;
    memcpy(&n, data.begin(), 8);
    out += "n=" + std::to_string(n);
  }
  auto ptrs = s.getPointerSection();
  int last = -1;
  for (int i = 0; i < int(ptrs.size()); ++i) {
    if (!ptrs[i].isNull()) last = i;
  }
  if (last >= 0) {
    out += out.empty() ? "p=" : " p=";
    for (int i = 0; i <= last; ++i) {
      if (ptrs[i].isNull()) out += "-";
      else if (ptrs[i].isCapability()) out += "c";
      else if (ptrs[i].isStruct()) out += "s";
      else out += "l";
    }
  }
  if (out.empty()) out = "{}";
  return out;
}

std::string payloadPeer(rpc::Payload::Reader p, std::vector<uint32_t>* got) {
  std::string s = contentStr(p.getContent()) + " caps[";
  bool first = true;
  for (auto cd : p.getCapTable()) {
    if (!first) s += " ";
    first = false;
    s += capDescPeer(cd, got);
  }
  return s + "]";
}

// ---------------------------------------------------------------- send helpers

void setOps(rpc::PromisedAnswer::Builder pa, const std::vector<int>& ops) {
  auto t = pa.initTransform(ops.size());
  for (size_t i = 0; i < ops.size(); ++i) {
    if (ops[i] < 0) t[i].setNoop();
    else t[i].setGetPointerField(ops[i]);
  }
}

std::string opsName(const std::vector<int>& ops) {
  std::string s;
  for (int o : ops) s += "," + (o < 0 ? std::string("noop") : std::to_string(o));
  return s;
}

void sendFinish(uint32_t q, bool releaseCaps, bool log) {
  capnp::MallocMessageBuilder mb;
  auto f = mb.initRoot<rpc::Message>().initFinish();
  f.setQuestionId(q);
  f.setReleaseResultCaps(releaseCaps);
  f.setRequireEarlyCancellationWorkaround(false);
  sendRaw(mb);
  if (q < questions.size()) {
    auto& qq = questions[q];
    if (qq.finished) return;
    qq.finished = true;
    qq.finishReleased = releaseCaps;
    if (releaseCaps && qq.returned) {
      for (auto w : qq.resultImports) dropImportRefs(w, 1);
      qq.resultImports.clear();
    }
  }
  if (log) emit("D> FIN q" + std::to_string(q) + " rrc=" + (releaseCaps ? "1" : "0"));
}

void sendReturnResults(uint32_t wire, int64_t n, bool withCap, bool promise = false) {
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initReturn();
  r.setAnswerId(wire);
  r.setReleaseParamCaps(false);
  auto res = r.initResults();
  std::string capStr;
  if (withCap) {
    uint32_t id = nextExportId++;
    exports[id].refs += 1;
    exports[id].promise = promise;
    capnp::BuilderCapabilityTable table;
    auto content = table.imbue(res.getContent());
    auto st = content.initAs<CapResult>();
    st.setCap(capnp::Capability::Client(capnp::newBrokenCap("placeholder")).castAs<Target>());
    if (promise) res.initCapTable(1)[0].setSenderPromise(id);
    else res.initCapTable(1)[0].setSenderHosted(id);
    capStr = (promise ? "sp:D" : "sh:D") + std::to_string(id);
    auto it = peerQs.find(wire);
    if (it != peerQs.end()) it->second.resultExports.push_back(id);
  } else {
    res.getContent().initAs<N>().setN(n);
  }
  sendRaw(mb);
  emit("D> RET " + peerQName(wire, false) + " res " +
       (withCap ? std::string("p=c") : "n=" + std::to_string(n)) + " caps[" + capStr + "]");
}

void sendReturnException(uint32_t wire, const char* reason) {
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initReturn();
  r.setAnswerId(wire);
  r.setReleaseParamCaps(false);
  auto e = r.initException();
  e.setType(rpc::Exception::Type::FAILED);
  e.setReason(reason);
  sendRaw(mb);
  emit("D> RET " + peerQName(wire, false) + " exc(" + reason + ")");
}

void sendReturnExceptionType(uint32_t wire, const char* reason, rpc::Exception::Type type,
                             const char* label) {
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initReturn();
  r.setAnswerId(wire);
  r.setReleaseParamCaps(false);
  auto e = r.initException();
  e.setType(type);
  e.setReason(reason);
  sendRaw(mb);
  emit("D> RET " + peerQName(wire, false) + " exc(" + label + ": " + reason + ")");
}

void sendReturnElsewhere(uint32_t wire) {
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initReturn();
  r.setAnswerId(wire);
  r.setReleaseParamCaps(false);
  r.setResultsSentElsewhere();
  sendRaw(mb);
  emit("D> RET " + peerQName(wire, false) + " elsewhere");
}

void markAnswered(uint32_t wire) {
  auto it = peerQs.find(wire);
  if (it != peerQs.end()) it->second.answered = true;
  retirePeerQ(wire);
}

// ---------------------------------------------------------------- receive

void peerReleases(uint32_t id, uint32_t count);

void onBootstrap(rpc::Bootstrap::Reader b) {
  uint32_t wire = b.getQuestionId();
  emit("P> BOOT " + peerQName(wire, true));
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initReturn();
  r.setAnswerId(wire);
  r.setReleaseParamCaps(false);
  auto res = r.initResults();
  capnp::BuilderCapabilityTable table;
  auto content = table.imbue(res.getContent());
  content.setAs<Callback>(capnp::Capability::Client(capnp::newBrokenCap("placeholder")).castAs<Callback>());
  res.initCapTable(1)[0].setSenderHosted(0);
  exports[0].refs += 1;
  peerQs[wire].resultExports.push_back(0);
  sendRaw(mb);
  emit("D> RET " + peerQName(wire, false) + " res p=c caps[sh:D0]");
  markAnswered(wire);
}

std::string targetPeer(rpc::MessageTarget::Reader t) {
  if (t.isImportedCap()) return "D" + std::to_string(t.getImportedCap());
  auto pa = t.getPromisedAnswer();
  return "pa(" + peerQName(pa.getQuestionId(), false) + opsStr(pa.getTransform()) + ")";
}

void onCall(rpc::Call::Reader c) {
  uint32_t wire = c.getQuestionId();
  std::string q = peerQName(wire, true);
  std::string iface = c.getInterfaceId() == CALLBACK_ID ? "Callback"
                      : c.getInterfaceId() == TARGET_ID ? "Target"
                                                        : "iface?";
  std::string to = "caller";
  bool yourself = false;
  switch (c.getSendResultsTo().which()) {
    case rpc::Call::SendResultsTo::CALLER: break;
    case rpc::Call::SendResultsTo::YOURSELF: to = "yourself"; yourself = true; break;
    case rpc::Call::SendResultsTo::THIRD_PARTY: to = "third"; break;
  }
  int64_t n = 0;
  auto content = c.getParams().getContent();
  if (content.isStruct()) {
    auto data = content.getAs<capnp::AnyStruct>().getDataSection();
    if (data.size() >= 8) memcpy(&n, data.begin(), 8);
  }
  emit("P> CALL " + q + " " + targetPeer(c.getTarget()) + " " + iface + "." +
       std::to_string(c.getMethodId()) + " to=" + to +
       (c.getNoPromisePipelining() ? " npp" : "") + (c.getOnlyPromisePipeline() ? " opp" : "") +
       " " + payloadPeer(c.getParams(), nullptr));
  pending.push_back({wire, yourself, n, ++incomingCount});
}

void onReturn(rpc::Return::Reader r) {
  uint32_t a = r.getAnswerId();
  bool known = a < questions.size();
  Question* q = known ? &questions[a] : nullptr;
  std::string head = "P> RET q" + std::to_string(a) + (r.getNoFinishNeeded() ? " nfn" : "");
  std::string body;
  std::vector<uint32_t> got;
  switch (r.which()) {
    case rpc::Return::RESULTS:
      body = "res " + payloadPeer(r.getResults(), &got);
      break;
    case rpc::Return::EXCEPTION:
      body = exceptionStr(r.getException());
      break;
    case rpc::Return::CANCELED:
      body = "canceled";
      break;
    case rpc::Return::RESULTS_SENT_ELSEWHERE:
      body = "elsewhere";
      break;
    case rpc::Return::TAKE_FROM_OTHER_QUESTION:
      body = "take " + peerQName(r.getTakeFromOtherQuestion(), false);
      break;
    case rpc::Return::ACCEPT_FROM_THIRD_PARTY:
      body = "await3";
      break;
  }
  if (q) {
    if (q->returned) head += " DUP";
    q->returned = true;
    for (auto w : got) q->resultImports.push_back(w);
    if (r.getReleaseParamCaps()) {
      for (auto e : q->paramExports) peerReleases(e, 1);
    }
    q->paramExports.clear();
    if (q->finished && q->finishReleased) {
      // Finished before the Return: the Finish released the result caps.
      for (auto w : q->resultImports) dropImportRefs(w, 1);
      q->resultImports.clear();
    }
    if (q->sync) return;
  } else {
    head += " UNKNOWN";
  }
  // releaseParamCaps is shown as well as the Releases it implies: a
  // Return with no param caps still sets it.
  if (r.getReleaseParamCaps()) body += " rpc=1";
  emit(head + " " + body);
}

void onFinish(rpc::Finish::Reader f) {
  uint32_t wire = f.getQuestionId();
  auto it = peerQs.find(wire);
  // Before the Return the flag says whether the caller drops the result
  // caps; after it, the flag shows up as the Releases it implies.
  std::string flag;
  if (it == peerQs.end() || !it->second.answered)
    flag = f.getReleaseResultCaps() ? " rrc=1" : " rrc=0";
  else if (f.getReleaseResultCaps() && it->second.resultExports.empty())
    flag = " rrc=1";
  emit("P> FIN " + peerQName(wire, false) + flag);
  if (it != peerQs.end()) {
    if (f.getReleaseResultCaps()) {
      for (auto id : it->second.resultExports) peerReleases(id, 1);
    }
    it->second.resultExports.clear();
    it->second.finished = true;
    retirePeerQ(wire);
  }
}

// A Release, or the release a Return.releaseParamCaps or a
// Finish.releaseResultCaps implies. Both log the same line.
void peerReleases(uint32_t id, uint32_t count) {
  std::string s = "P> REL D" + std::to_string(id) + " n=" + std::to_string(count);
  auto it = exports.find(id);
  if (it == exports.end()) {
    s += " UNKNOWN";
  } else {
    it->second.refs -= int(count);
    if (it->second.refs < 0) s += " OVER";
  }
  emit(s);
}

void onRelease(rpc::Release::Reader r) {
  peerReleases(r.getId(), r.getReferenceCount());
}

void onResolve(rpc::Resolve::Reader r) {
  std::string s = "P> RES " + importRef(r.getPromiseId()) + " ";
  if (r.isCap()) s += capDescPeer(r.getCap(), nullptr);
  else s += exceptionStr(r.getException());
  emit(s);
}

void onDisembargo(rpc::Disembargo::Reader d) {
  std::string tgt;
  auto t = d.getTarget();
  if (t.isImportedCap()) tgt = "D" + std::to_string(t.getImportedCap());
  else tgt = "pa(" + peerQName(t.getPromisedAnswer().getQuestionId(), false) +
             opsStr(t.getPromisedAnswer().getTransform()) + ")";
  auto ctx = d.getContext();
  switch (ctx.which()) {
    case rpc::Disembargo::Context::SENDER_LOOPBACK: {
      emit("P> DIS " + tgt + " sl " + embargoName(ctx.getSenderLoopback()));
      // Reflect it to the capability the promise resolved to.
      capnp::MallocMessageBuilder mb;
      auto o = mb.initRoot<rpc::Message>().initDisembargo();
      uint32_t back = 0;
      bool ok = false;
      if (t.isImportedCap()) {
        auto it = exports.find(t.getImportedCap());
        if (it != exports.end() && it->second.resolvedToPeer) {
          back = it->second.peerWire;
          ok = true;
        }
      }
      if (!ok) {
        emit("D> (no reflection target)");
        return;
      }
      o.initTarget().setImportedCap(back);
      o.getContext().setReceiverLoopback(ctx.getSenderLoopback());
      sendRaw(mb);
      emit("D> DIS " + importRef(back) + " rl " + embargoName(ctx.getSenderLoopback()));
      break;
    }
    case rpc::Disembargo::Context::RECEIVER_LOOPBACK:
      emit("P> DIS " + tgt + " rl " + std::to_string(ctx.getReceiverLoopback()));
      break;
    default:
      emit("P> DIS " + tgt + " other");
  }
}

void handle(InMsg& m) {
  auto msg = m.reader->getRoot<rpc::Message>();
  switch (msg.which()) {
    case rpc::Message::BOOTSTRAP: onBootstrap(msg.getBootstrap()); break;
    case rpc::Message::CALL: onCall(msg.getCall()); break;
    case rpc::Message::RETURN: onReturn(msg.getReturn()); break;
    case rpc::Message::FINISH: onFinish(msg.getFinish()); break;
    case rpc::Message::RELEASE: onRelease(msg.getRelease()); break;
    case rpc::Message::RESOLVE: onResolve(msg.getResolve()); break;
    case rpc::Message::DISEMBARGO: onDisembargo(msg.getDisembargo()); break;
    case rpc::Message::ABORT:
      emit("P> ABORT " + exceptionStr(msg.getAbort()));
      peerAborted = true;
      break;
    case rpc::Message::UNIMPLEMENTED:
      emit("P> UNIMPL " + std::to_string(int(msg.getUnimplemented().which())));
      break;
    default:
      emit("P> MSG " + std::to_string(int(msg.which())));
  }
}

// ---------------------------------------------------------------- sync

uint32_t newQuestion(const std::string& what) {
  Question q;
  q.id = questions.size();
  q.what = what;
  questions.push_back(q);
  return q.id;
}

void drainIdle(int ms) {
  InMsg m;
  while (!peerEof && recvOne(m, ms)) handle(m);
}

int syncSeq = 0;
int syncTimeouts = 0;

// Sync call on the bootstrap import, then an idle wait.
void sync() {
  if (peerEof) return;
  if (!haveBoot || imports.find(bootImportWire) == imports.end()) {
    drainIdle(kIdleMs * 4);
    return;
  }
  uint32_t q = newQuestion("sync");
  questions[q].sync = true;
  {
    capnp::MallocMessageBuilder mb;
    auto c = mb.initRoot<rpc::Message>().initCall();
    c.setQuestionId(q);
    c.setInterfaceId(TARGET_ID);
    c.setMethodId(0);
    c.initTarget().setImportedCap(bootImportWire);
    c.initParams().getContent().initAs<N>().setN(-(++syncSeq));
    sendRaw(mb);
  }
  auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(kSyncTimeoutMs);
  InMsg m;
  while (!peerEof && !questions[q].returned) {
    auto left = std::chrono::duration_cast<std::chrono::milliseconds>(
                    deadline - std::chrono::steady_clock::now())
                    .count();
    if (left <= 0) break;
    if (recvOne(m, int(left))) handle(m);
  }
  if (!questions[q].returned && !peerEof) {
    emit("D> SYNC-TIMEOUT");
    ++syncTimeouts;
  }
  if (!peerEof) sendFinish(q, true, false);
  drainIdle(kIdleMs);
}

// ---------------------------------------------------------------- actions

int pick(int n) { return n <= 0 ? 0 : int(rng() % uint64_t(n)); }
bool chance(int pct) { return int(rng() % 100) < pct; }

std::vector<uint32_t> liveImports(bool includeBoot) {
  std::vector<std::pair<int, uint32_t>> v;
  for (auto& kv : imports) {
    if (!includeBoot && haveBoot && kv.first == bootImportWire) continue;
    v.push_back({kv.second.sym, kv.first});
  }
  std::sort(v.begin(), v.end());
  std::vector<uint32_t> out;
  for (auto& p : v) out.push_back(p.second);
  return out;
}

std::vector<int> randomOps() {
  switch (pick(10)) {
    case 0: case 1: return {};
    case 2: return {-1, 0};
    case 3: return {0, 0};
    case 4: return {pick(20)};
    case 5: return {1};
    default: return {0};
  }
}

const char* kMethods[] = {"echo", "fail", "make", "slow", "self", "reflect", "many", "slowMake"};

void actCall(std::string& label) {
  static const int weights[] = {25, 10, 15, 15, 8, 10, 7, 6};
  int total = 0;
  for (int w : weights) total += w;
  int r = pick(total), method = 0;
  for (; method < 7; ++method) {
    if (r < weights[method]) break;
    r -= weights[method];
  }
  int64_t n = method == 6 ? 1 + pick(20) : pick(100);
  uint32_t q = newQuestion(kMethods[method]);
  capnp::MallocMessageBuilder mb;
  auto c = mb.initRoot<rpc::Message>().initCall();
  c.setQuestionId(q);
  c.setInterfaceId(TARGET_ID);
  c.setMethodId(method);
  std::string tgt;
  auto imps = liveImports(true);
  int tsel = pick(100);
  if (tsel < 3) {
    c.initTarget().setImportedCap(4000 + pick(10));
    tgt = "imp?";
  } else if (tsel < 60 && !imps.empty()) {
    uint32_t w = imps[pick(imps.size())];
    c.initTarget().setImportedCap(w);
    tgt = importRef(w);
  } else {
    uint32_t aq = questions.size() > 1 ? pick(questions.size() - 1) : 0;
    if (chance(4)) aq = 5000 + pick(10);
    auto ops = randomOps();
    auto pa = c.initTarget().initPromisedAnswer();
    pa.setQuestionId(aq);
    setOps(pa, ops);
    tgt = "pa(q" + std::to_string(aq) + opsName(ops) + ")";
  }
  std::string flags;
  if (chance(10)) {
    c.getSendResultsTo().setYourself();
    flags += " yourself";
  } else {
    c.getSendResultsTo().setCaller();
  }
  if (chance(10)) {
    c.setNoPromisePipelining(true);
    flags += " npp";
  }
  if (chance(4)) {
    c.setOnlyPromisePipeline(true);
    flags += " opp";
  }
  auto params = c.initParams();
  std::string caps;
  capnp::BuilderCapabilityTable table;
  if (chance(15)) {
    // Content points at the first cap. A second one is in the table only.
    int k = 1 + pick(2);
    auto st = table.imbue(params.getContent()).initAs<NCap>();
    st.setN(n);
    st.setCap(capnp::Capability::Client(capnp::newBrokenCap("placeholder")).castAs<Target>());
    auto ct = params.initCapTable(k);
    for (int i = 0; i < k; ++i) {
      int kind = pick(3);
      if (kind == 2 && imps.empty()) kind = 0;
      if (i) caps += " ";
      if (kind == 0 || kind == 1) {
        uint32_t id = nextExportId++;
        exports[id].refs += 1;
        exports[id].promise = kind == 1;
        if (kind == 0) ct[i].setSenderHosted(id);
        else ct[i].setSenderPromise(id);
        questions[q].paramExports.push_back(id);
        caps += (kind == 0 ? "sh:D" : "sp:D") + std::to_string(id);
      } else {
        uint32_t w = imps[pick(imps.size())];
        ct[i].setReceiverHosted(w);
        caps += "rh:" + importRef(w);
      }
    }
  } else {
    params.getContent().initAs<N>().setN(n);
  }
  sendRaw(mb);
  label = "call";
  emit("D> CALL q" + std::to_string(q) + " " + tgt + " " + kMethods[method] + "(" +
       std::to_string(n) + ")" + flags + " caps[" + caps + "]");
}

void actFinish(std::string& label) {
  label = "finish";
  if (chance(3)) {
    uint32_t q = 6000 + pick(10);
    sendFinish(q, false, true);
    return;
  }
  std::vector<uint32_t> cand;
  bool wantReturned = chance(70);
  for (auto& q : questions) {
    if (q.finished || q.sync || q.what == "boot0") continue;
    if (wantReturned && !q.returned) continue;
    cand.push_back(q.id);
  }
  if (cand.empty()) {
    for (auto& q : questions)
      if (!q.finished && !q.sync && q.what != "boot0") cand.push_back(q.id);
  }
  if (cand.empty()) return;
  sendFinish(cand[pick(cand.size())], chance(50), true);
}

void sendRelease(uint32_t wire, int count, bool log) {
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initRelease();
  r.setId(wire);
  r.setReferenceCount(count);
  sendRaw(mb);
  if (log) emit("D> REL " + importRef(wire) + " n=" + std::to_string(count));
  dropImportRefs(wire, count);
}

void actRelease(std::string& label) {
  label = "release";
  if (chance(3)) {
    capnp::MallocMessageBuilder mb;
    auto r = mb.initRoot<rpc::Message>().initRelease();
    r.setId(7000 + pick(10));
    r.setReferenceCount(1);
    sendRaw(mb);
    emit("D> REL imp? n=1");
    return;
  }
  auto imps = liveImports(false);
  if (imps.empty()) return;
  uint32_t w = imps[pick(imps.size())];
  int refs = imports[w].refs;
  sendRelease(w, chance(30) ? refs : 1, true);
}

void actAnswer(std::string& label) {
  label = "answer";
  if (pending.empty()) return;
  int i = pick(pending.size());
  Incoming in = pending[i];
  pending.erase(pending.begin() + i);
  if (in.yourself) {
    sendReturnElsewhere(in.wire);
  } else if (chance(12)) {
    sendReturnException(in.wire, "PINGEXC");
  } else {
    sendReturnResults(in.wire, in.n + 1000, chance(20));
  }
  markAnswered(in.wire);
}

void actPoke(std::string& label) {
  label = "poke70";
  if (!haveBoot) return;
  bool slow = chance(30);
  for (int i = 0; i < 70; ++i) {
    uint32_t q = newQuestion(slow ? "slow" : "echo");
    capnp::MallocMessageBuilder mb;
    auto c = mb.initRoot<rpc::Message>().initCall();
    c.setQuestionId(q);
    c.setInterfaceId(TARGET_ID);
    c.setMethodId(slow ? 3 : 0);
    c.initTarget().setImportedCap(bootImportWire);
    c.initParams().getContent().initAs<N>().setN(i);
    sendRaw(mb);
  }
  emit(std::string("D> POKE70 ") + (slow ? "slow" : "echo") + " q" +
       std::to_string(questions.size() - 70) + "..q" + std::to_string(questions.size() - 1));
}

void actResolve(std::string& label) {
  label = "resolve";
  std::vector<uint32_t> cand;
  for (auto& kv : exports)
    if (kv.second.promise && !kv.second.resolved) cand.push_back(kv.first);
  uint32_t pid;
  if (cand.empty() || chance(15)) pid = kNoSuchPromise + pick(10);
  else pid = cand[pick(cand.size())];
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initResolve();
  r.setPromiseId(pid);
  std::string what;
  auto imps = liveImports(true);
  int kind = pick(5);
  if (kind == 1 && imps.empty()) kind = 0;
  auto& ex = exports[pid];
  switch (kind) {
    case 0: {
      uint32_t id = nextExportId++;
      exports[id].refs += 1;
      r.initCap().setSenderHosted(id);
      what = "sh:D" + std::to_string(id);
      break;
    }
    case 1: {
      uint32_t w = imps[pick(imps.size())];
      r.initCap().setReceiverHosted(w);
      ex.resolvedToPeer = true;
      ex.peerWire = w;
      what = "rh:" + importRef(w);
      break;
    }
    case 2: {
      uint32_t aq = questions.empty() ? 0 : pick(questions.size());
      auto ops = randomOps();
      auto pa = r.initCap().initReceiverAnswer();
      pa.setQuestionId(aq);
      setOps(pa, ops);
      what = "ra:(q" + std::to_string(aq) + opsName(ops) + ")";
      break;
    }
    case 3: {
      auto e = r.initException();
      e.setType(rpc::Exception::Type::FAILED);
      e.setReason("RESOLVEEXC");
      what = "exc(RESOLVEEXC)";
      break;
    }
    default: {
      uint32_t id = nextExportId++;
      exports[id].refs += 1;
      exports[id].promise = true;
      r.initCap().setSenderPromise(id);
      what = "sp:D" + std::to_string(id);
    }
  }
  ex.resolved = true;
  if (pid >= kNoSuchPromise) exports.erase(pid);
  sendRaw(mb);
  emit("D> RES D" + std::to_string(pid) + " " + what);
}

void actDisembargo(std::string& label) {
  label = "disembargo";
  capnp::MallocMessageBuilder mb;
  auto d = mb.initRoot<rpc::Message>().initDisembargo();
  auto imps = liveImports(true);
  if (chance(50) || imps.empty()) {
    d.initTarget().setImportedCap(imps.empty() ? 0 : imps[0]);
    d.getContext().setReceiverLoopback(8000 + pick(10));
    emit("D> DIS " + (imps.empty() ? std::string("imp0") : importRef(imps[0])) + " rl ?");
  } else {
    uint32_t w = imps[pick(imps.size())];
    d.initTarget().setImportedCap(w);
    d.getContext().setSenderLoopback(8100 + pick(10));
    emit("D> DIS " + importRef(w) + " sl ?");
  }
  sendRaw(mb);
}

void actBoot(std::string& label) {
  label = "bootstrap";
  uint32_t q = newQuestion("boot");
  capnp::MallocMessageBuilder mb;
  mb.initRoot<rpc::Message>().initBootstrap().setQuestionId(q);
  sendRaw(mb);
  emit("D> BOOT q" + std::to_string(q));
}

void actUnknownMethod(std::string& label) {
  label = "unknown_method";
  auto imps = liveImports(true);
  if (imps.empty()) return;
  uint32_t w = imps[pick(imps.size())];
  uint32_t q = newQuestion("unknown_method");
  capnp::MallocMessageBuilder mb;
  auto c = mb.initRoot<rpc::Message>().initCall();
  c.setQuestionId(q);
  c.initTarget().setImportedCap(w);
  std::string what;
  if (chance(50)) {
    uint16_t mid = uint16_t(8 + pick(20));
    c.setInterfaceId(TARGET_ID);
    c.setMethodId(mid);
    what = "Target." + std::to_string(mid);
  } else {
    uint64_t iid = chance(50) ? (0x123456789abcdef0ull + pick(5))
                              : (0x9000000000000001ull + pick(5));
    uint16_t mid = uint16_t(pick(5));
    c.setInterfaceId(iid);
    c.setMethodId(mid);
    what = "iface(" + std::to_string(iid) + ")." + std::to_string(mid);
  }
  c.initParams().getContent().initAs<N>().setN(pick(100));
  sendRaw(mb);
  emit("D> CALL q" + std::to_string(q) + " " + importRef(w) + " " + what);
}

void actSelfPipeline(std::string& label) {
  label = "self_pipeline";
  uint32_t q = newQuestion("self_pipe");
  auto ops = randomOps();
  capnp::MallocMessageBuilder mb;
  auto c = mb.initRoot<rpc::Message>().initCall();
  c.setQuestionId(q);
  c.setInterfaceId(TARGET_ID);
  c.setMethodId(0);
  auto pa = c.initTarget().initPromisedAnswer();
  pa.setQuestionId(q);
  setOps(pa, ops);
  c.initParams().getContent().initAs<N>().setN(pick(100));
  sendRaw(mb);
  emit("D> CALL q" + std::to_string(q) + " pa(q" + std::to_string(q) + opsName(ops) + ") echo");
}

void actAbort(std::string& label) {
  label = "abort";
  static const rpc::Exception::Type types[] = {
      rpc::Exception::Type::FAILED,
      rpc::Exception::Type::OVERLOADED,
      rpc::Exception::Type::DISCONNECTED,
      rpc::Exception::Type::UNIMPLEMENTED,
  };
  static const char* reasons[] = {
      "driver abort",
      "remote exception: driver abort",
  };
  auto ty = types[pick(4)];
  const char* reason = reasons[pick(2)];
  capnp::MallocMessageBuilder mb;
  auto ex = mb.initRoot<rpc::Message>().initAbort();
  ex.setType(ty);
  ex.setReason(reason);
  sendRaw(mb);
  emit("D> ABORT " + exceptionStr(ex.asReader()));
  driverAborted = true;
}

void waitPeerClose() {
  auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(kSyncTimeoutMs);
  InMsg m;
  while (!peerEof && std::chrono::steady_clock::now() < deadline) {
    auto left = std::chrono::duration_cast<std::chrono::milliseconds>(
                    deadline - std::chrono::steady_clock::now())
                    .count();
    if (left <= 0) break;
    if (recvOne(m, int(left))) handle(m);
  }
}

// ---------------------------------------------------------------- scenarios
//
// Fixed shapes that random steps reach too rarely. Each part is its own
// window, marked "== <step>.<part> <CLASS>", so the runner can count
// seeds per class and blame a first difference on one.

uint32_t callOnBoot(int method, int64_t n) {
  uint32_t q = newQuestion(kMethods[method]);
  capnp::MallocMessageBuilder mb;
  auto c = mb.initRoot<rpc::Message>().initCall();
  c.setQuestionId(q);
  c.setInterfaceId(TARGET_ID);
  c.setMethodId(method);
  c.initTarget().setImportedCap(bootImportWire);
  c.initParams().getContent().initAs<N>().setN(n);
  sendRaw(mb);
  emit("D> CALL q" + std::to_string(q) + " " + importRef(bootImportWire) + " " +
       kMethods[method] + "(" + std::to_string(n) + ") caps[]");
  return q;
}

// A call on pa(aq, [0]). raQ >= 0 adds a receiverAnswer(raQ, [0]) param,
// and sh adds a senderHosted param naming a fresh driver export.
uint32_t callOnPipeline(uint32_t aq, int method, int64_t n, int64_t raQ, bool sh = false) {
  uint32_t q = newQuestion(kMethods[method]);
  capnp::MallocMessageBuilder mb;
  auto c = mb.initRoot<rpc::Message>().initCall();
  c.setQuestionId(q);
  c.setInterfaceId(TARGET_ID);
  c.setMethodId(method);
  auto pa = c.initTarget().initPromisedAnswer();
  pa.setQuestionId(aq);
  setOps(pa, {0});
  auto params = c.initParams();
  std::string caps;
  capnp::BuilderCapabilityTable table;
  if (raQ >= 0 || sh) {
    auto st = table.imbue(params.getContent()).initAs<NCap>();
    st.setN(n);
    st.setCap(capnp::Capability::Client(capnp::newBrokenCap("placeholder")).castAs<Target>());
  } else {
    params.getContent().initAs<N>().setN(n);
  }
  if (raQ >= 0) {
    auto ra = params.initCapTable(1)[0].initReceiverAnswer();
    ra.setQuestionId(uint32_t(raQ));
    setOps(ra, {0});
    caps = "ra:(q" + std::to_string(raQ) + ",0)";
  } else if (sh) {
    uint32_t id = nextExportId++;
    exports[id].refs += 1;
    params.initCapTable(1)[0].setSenderHosted(id);
    questions[q].paramExports.push_back(id);
    caps = "sh:D" + std::to_string(id);
  }
  sendRaw(mb);
  emit("D> CALL q" + std::to_string(q) + " pa(q" + std::to_string(aq) + ",0) " +
       kMethods[method] + "(" + std::to_string(n) + ") caps[" + caps + "]");
  return q;
}

int scenarioPart = 0;
std::string scenarioStep;

void part(const std::string& cls) {
  std::vector<std::string> acted = window;
  window.clear();
  printf("== %s.%d %s\n", scenarioStep.c_str(), ++scenarioPart, cls.c_str());
  for (auto& s : acted) printf("%s\n", s.c_str());
  sync();
  flushWindow();
}

void partAbort(const std::string& cls) {
  std::vector<std::string> acted = window;
  window.clear();
  printf("== %s.%d %s\n", scenarioStep.c_str(), ++scenarioPart, cls.c_str());
  for (auto& s : acted) printf("%s\n", s.c_str());
  waitPeerClose();
  flushWindow();
}

// The ping a slow call made, as the newest pending call since `before`.
int64_t newPing(size_t before) {
  return pending.size() > before ? int64_t(pending.back().wire) : -1;
}

void answerWire(int64_t wire, int64_t n) {
  for (size_t i = 0; i < pending.size(); ++i) {
    if (int64_t(pending[i].wire) != wire) continue;
    pending.erase(pending.begin() + i);
    sendReturnResults(uint32_t(wire), n, false);
    markAnswered(uint32_t(wire));
    return;
  }
  emit("D> (no such ping)");
}

const char* kScenarios[] = {"FINISH_QUEUED_Q2", "EARLY_FINISH_Q1_WITH_Q2", "FWD_RA_PARAM",
                            "DISEMBARGO_FWD", "FWD_SH_PARAM"};

void scenario(int k, int which) {
  std::string cls = kScenarios[which];
  scenarioStep = std::to_string(k);
  scenarioPart = 0;
  window.clear();
  if (!haveBoot) return;
  int64_t n = pick(100);
  switch (which) {
    case 0: {
      // Finish(q2) while q2 waits on the pending answer q1: q2 is
      // canceled at once and never runs, so it never pings.
      size_t before = pending.size();
      uint32_t q1 = callOnBoot(7, n);
      part(cls);
      int64_t ping = newPing(before);
      uint32_t q2 = callOnPipeline(q1, 3, n + 1, -1);
      part(cls);
      sendFinish(q2, false, true);
      part(cls);
      answerWire(ping, n + 1000);
      part(cls);
      break;
    }
    case 1: {
      // Finish(q1) before it returns, with q2 pipelined on it: q1 is
      // canceled and q2 still reaches q1's result.
      size_t before = pending.size();
      uint32_t q1 = callOnBoot(7, n);
      part(cls);
      int64_t ping = newPing(before);
      callOnPipeline(q1, 0, n + 1, -1);
      part(cls);
      sendFinish(q1, false, true);
      part(cls);
      answerWire(ping, n + 1000);
      part(cls);
      break;
    }
    case 2: {
      // A forwarded call carries a receiverAnswer param.
      uint32_t qm = callOnBoot(2, n);
      part(cls);
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      callOnPipeline(qr, 0, n + 2, qm);
      part(cls);
      break;
    }
    case 4: {
      // A forwarded call carries a senderHosted param. The parts show
      // when the forwarding side releases it.
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      size_t before = pending.size();
      uint32_t q3 = callOnPipeline(qr, 0, n + 4, -1, true);
      part(cls);
      int64_t fwd = newPing(before);
      if (fwd >= 0) {
        for (size_t i = 0; i < pending.size(); ++i)
          if (int64_t(pending[i].wire) == fwd) pending.erase(pending.begin() + i);
        sendReturnElsewhere(uint32_t(fwd));
        markAnswered(uint32_t(fwd));
      }
      part(cls);
      sendFinish(q3, false, true);
      part(cls);
      break;
    }
    default: {
      // Disembargo(senderLoopback) on an answer that was forwarded.
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      uint32_t q3 = callOnPipeline(qr, 0, n + 3, -1);
      part(cls);
      capnp::MallocMessageBuilder mb;
      auto d = mb.initRoot<rpc::Message>().initDisembargo();
      auto pa = d.initTarget().initPromisedAnswer();
      pa.setQuestionId(q3);
      setOps(pa, {});
      d.getContext().setSenderLoopback(777 + k);
      sendRaw(mb);
      emit("D> DIS pa(q" + std::to_string(q3) + ") sl " + std::to_string(777 + k));
      part(cls);
      break;
    }
  }
}

// ---------------------------------------------------------------- probes
//
// Fixed scripts from review, one per name. Each part is a window marked
// "== probe.<part> <NAME>".

struct Cap {
  char kind;  // 'h' senderHosted, 'p' senderPromise, 'a' receiverAnswer,
              // 'r' receiverHosted (resolve only)
  uint32_t id;
  std::vector<int> ops;
};

Cap hostedCap() {
  uint32_t id = nextExportId++;
  exports[id].refs += 1;
  return {'h', id, {}};
}

Cap promiseCap() {
  uint32_t id = nextExportId++;
  exports[id].refs += 1;
  exports[id].promise = true;
  return {'p', id, {}};
}

// A call with an explicit target: the bootstrap import when aq is -1,
// import wire -2 - aq below that, else pa(aq, ops). Params point at the
// first cap, if any.
uint32_t probeCall(int64_t aq, std::vector<int> ops, int method, int64_t n, bool yourself,
                   bool npp, std::vector<Cap> caps, bool opp = false) {
  uint32_t q = newQuestion(kMethods[method]);
  capnp::MallocMessageBuilder mb;
  auto c = mb.initRoot<rpc::Message>().initCall();
  c.setQuestionId(q);
  c.setInterfaceId(TARGET_ID);
  c.setMethodId(method);
  std::string tgt;
  if (aq < 0) {
    uint32_t w = aq == -1 ? bootImportWire : uint32_t(-2 - aq);
    c.initTarget().setImportedCap(w);
    tgt = importRef(w);
  } else {
    auto pa = c.initTarget().initPromisedAnswer();
    pa.setQuestionId(uint32_t(aq));
    setOps(pa, ops);
    tgt = "pa(q" + std::to_string(aq) + opsName(ops) + ")";
  }
  std::string flags;
  if (yourself) {
    c.getSendResultsTo().setYourself();
    flags += " yourself";
  }
  if (npp) {
    c.setNoPromisePipelining(true);
    flags += " npp";
  }
  if (opp) {
    c.setOnlyPromisePipeline(true);
    flags += " opp";
  }
  auto params = c.initParams();
  capnp::BuilderCapabilityTable table;
  std::string cs;
  if (caps.empty()) {
    params.getContent().initAs<N>().setN(n);
  } else {
    auto st = table.imbue(params.getContent()).initAs<NCap>();
    st.setN(n);
    st.setCap(capnp::Capability::Client(capnp::newBrokenCap("placeholder")).castAs<Target>());
    auto ct = params.initCapTable(caps.size());
    for (size_t i = 0; i < caps.size(); ++i) {
      if (i) cs += " ";
      auto& k = caps[i];
      if (k.kind == 'h') {
        ct[i].setSenderHosted(k.id);
        questions[q].paramExports.push_back(k.id);
        cs += "sh:D" + std::to_string(k.id);
      } else if (k.kind == 'p') {
        ct[i].setSenderPromise(k.id);
        questions[q].paramExports.push_back(k.id);
        cs += "sp:D" + std::to_string(k.id);
      } else if (k.kind == 't' || k.kind == 'T') {
        // thirdPartyHosted with a vine and an id the peer must refuse: a
        // null id ('t') or a vat host longer than any it keeps ('T').
        auto tp = ct[i].initThirdPartyHosted();
        tp.setVineId(k.id);
        if (k.kind == 'T') {
          auto id = tp.getId().initAsAnyStruct(1, 1);
          auto vat = id.getPointerSection()[0].initAsAnyStruct(1, 1);
          vat.getPointerSection()[0].setAs<capnp::Text>(std::string(300, 'x').c_str());
        }
        cs += "tph:D" + std::to_string(k.id);
      } else {
        auto ra = ct[i].initReceiverAnswer();
        ra.setQuestionId(k.id);
        setOps(ra, k.ops);
        cs += "ra:(q" + std::to_string(k.id) + opsName(k.ops) + ")";
      }
    }
  }
  sendRaw(mb);
  emit("D> CALL q" + std::to_string(q) + " " + tgt + " " + kMethods[method] + "(" +
       std::to_string(n) + ")" + flags + " caps[" + cs + "]");
  return q;
}

// Resolve driver promise pid to an existing export ('h'), an existing
// promise ('p'), receiverAnswer(id, ops) ('a') or the peer's export
// wire id ('r').
void probeResolve(uint32_t pid, Cap to) {
  capnp::MallocMessageBuilder mb;
  auto r = mb.initRoot<rpc::Message>().initResolve();
  r.setPromiseId(pid);
  std::string what;
  if (to.kind == 'h') {
    exports[to.id].refs += 1;
    r.initCap().setSenderHosted(to.id);
    what = "sh:D" + std::to_string(to.id);
  } else if (to.kind == 'p') {
    exports[to.id].refs += 1;
    exports[to.id].promise = true;
    r.initCap().setSenderPromise(to.id);
    what = "sp:D" + std::to_string(to.id);
  } else if (to.kind == 'r') {
    r.initCap().setReceiverHosted(to.id);
    what = "rh:" + importRef(to.id);
  } else {
    auto pa = r.initCap().initReceiverAnswer();
    pa.setQuestionId(to.id);
    setOps(pa, to.ops);
    what = "ra:(q" + std::to_string(to.id) + opsName(to.ops) + ")";
  }
  exports[pid].resolved = true;
  sendRaw(mb);
  emit("D> RES D" + std::to_string(pid) + " " + what);
}

void probeDisembargo(uint32_t aq, std::vector<int> ops, uint32_t id) {
  capnp::MallocMessageBuilder mb;
  auto d = mb.initRoot<rpc::Message>().initDisembargo();
  auto pa = d.initTarget().initPromisedAnswer();
  pa.setQuestionId(aq);
  setOps(pa, ops);
  d.getContext().setSenderLoopback(id);
  sendRaw(mb);
  emit("D> DIS pa(q" + std::to_string(aq) + opsName(ops) + ") sl " + std::to_string(id));
}

int64_t importWireBySym(int sym) {
  for (auto& kv : imports)
    if (kv.second.sym == sym) return kv.first;
  return -1;
}

// Answer the newest pending call since `before`: with a cap, with a
// plain N, or with resultsSentElsewhere for a yourself call.
void probeAnswer(size_t before, bool withCap) {
  if (pending.size() <= before) {
    emit("D> (no pending call)");
    return;
  }
  Incoming in = pending.back();
  pending.pop_back();
  if (in.yourself) sendReturnElsewhere(in.wire);
  else sendReturnResults(in.wire, in.n + 1000, withCap);
  markAnswered(in.wire);
}

// ---------------------------------------------------------------- forced classes
//
// Shapes from review that the random steps above never build. They run
// only with "force=<CLASS>" or "mix" on the command line, so the plain
// seed scripts stay as they were.

const char* kForced[] = {"NPP_OPP_FWD",   "RA_RUNNING",   "RESOLVE_REFLECTED_RA",
                         "RELEASED_EXPORT_REUSE", "RA_REWRITE", "HINTS_FWD_RA",
                         "PLAIN_FWD_PROMISE_RESULT"};
const int kNumForced = 7;
int forceClass = -1;
bool mixForced = false;

// The newest call the peer made since `before`, by arrival serial.
int64_t newIncoming(size_t before) {
  return pending.size() > before ? int64_t(pending.back().serial) : -1;
}

// Answer the call with arrival serial `serial`, if it is still pending.
// Returns the driver export a promise result names, or 0.
uint32_t answerPending(int64_t serial, bool withCap, bool promise = false) {
  for (size_t i = 0; i < pending.size(); ++i) {
    if (int64_t(pending[i].serial) != serial) continue;
    Incoming in = pending[i];
    pending.erase(pending.begin() + i);
    uint32_t pid = 0;
    if (in.yourself) {
      sendReturnElsewhere(in.wire);
    } else if (chance(15)) {
      sendReturnException(in.wire, "PINGEXC");
    } else {
      sendReturnResults(in.wire, in.n + 1000, withCap || promise, promise);
      if (promise) pid = nextExportId - 1;
    }
    markAnswered(in.wire);
    return pid;
  }
  return 0;
}

void forcedScenario(int k, int which) {
  std::string cls = kForced[which];
  scenarioStep = std::to_string(k);
  scenarioPart = 0;
  window.clear();
  if (!haveBoot) return;
  int64_t n = pick(100);
  switch (which) {
    case 0: {
      // A forwarded call with noPromisePipelining and onlyPromisePipeline,
      // then a Disembargo, a pipelined call, the answer and a Finish in a
      // random order.
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      size_t before = pending.size();
      bool npp = chance(80), opp = chance(80);
      uint32_t q = probeCall(qr, {0}, chance(70) ? 0 : 2, n + 1, false, npp, {}, opp);
      part(cls);
      int64_t fwd = newIncoming(before);
      std::vector<int> acts = {0, 1, 2, 3};
      for (size_t i = acts.size(); i > 1; --i) std::swap(acts[i - 1], acts[pick(int(i))]);
      for (int a : acts) {
        if (a == 0 && chance(80)) {
          probeDisembargo(q, chance(70) ? std::vector<int>{0} : std::vector<int>{}, 700 + k);
        } else if (a == 1 && chance(50)) {
          callOnPipeline(q, chance(50) ? 0 : 2, n + 2, -1);
        } else if (a == 2 && chance(80)) {
          answerPending(fwd, chance(50));
        } else if (a == 3 && chance(60)) {
          sendFinish(q, chance(50), true);
        } else {
          continue;
        }
        part(cls);
      }
      break;
    }
    case 1: {
      // A receiverAnswer param names an answer that is still running.
      size_t before = pending.size();
      uint32_t q1 = callOnBoot(chance(50) ? 7 : 3, n);
      part(cls);
      int64_t ping = newIncoming(before);
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      size_t before2 = pending.size();
      std::vector<int> ops = chance(80) ? std::vector<int>{0} : std::vector<int>{};
      uint32_t q3 = probeCall(qr, {0}, 0, n + 2, chance(30), chance(30), {{'a', q1, ops}});
      part(cls);
      int64_t fwd = newIncoming(before2);
      std::vector<int> acts = {0, 1, 2, 3};
      for (size_t i = acts.size(); i > 1; --i) std::swap(acts[i - 1], acts[pick(int(i))]);
      for (int a : acts) {
        if (a == 0) answerPending(ping, false);
        else if (a == 1 && chance(70)) answerPending(fwd, chance(30));
        else if (a == 2 && chance(40)) sendFinish(q3, chance(50), true);
        else if (a == 3 && chance(40)) sendFinish(q1, chance(50), true);
        else continue;
        part(cls);
      }
      break;
    }
    case 2: {
      // A promise param resolves to receiverAnswer on an answer that
      // reflects the driver's own cap back.
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      Cap p = promiseCap();
      size_t before = pending.size();
      bool fwdCall = chance(75);
      uint32_t q3;
      if (fwdCall) q3 = probeCall(qr, {0}, 0, n + 1, chance(40), chance(30), {p});
      else q3 = probeCall(-1, {}, 4, n + 1, false, false, {p});
      part(cls);
      int64_t fwd = newIncoming(before);
      if (chance(30)) {
        answerPending(fwd, false);
        part(cls);
      }
      uint32_t to = chance(80) ? qr : q3;
      probeResolve(p.id, {'a', to, chance(85) ? std::vector<int>{0} : std::vector<int>{}});
      part(cls);
      answerPending(fwd, false);
      part(cls);
      if (chance(50)) {
        sendFinish(q3, chance(50), true);
        part(cls);
      }
      break;
    }
    case 3: {
      // A result cap is released while a call on it still runs, and a
      // new export takes its id. A Finish then releases result caps.
      uint32_t q2 = callOnBoot(2, n);
      part(cls);
      size_t before = pending.size();
      callOnPipeline(q2, chance(50) ? 7 : 3, n + 1, -1);
      part(cls);
      int64_t ping = newIncoming(before);
      if (!questions[q2].resultImports.empty()) {
        uint32_t w = questions[q2].resultImports[0];
        if (imports.count(w)) sendRelease(w, chance(70) ? 1 : imports[w].refs, true);
      }
      part(cls);
      if (chance(60)) callOnPipeline(q2, 2, n + 2, -1);
      else callOnBoot(2, n + 2);
      part(cls);
      sendFinish(q2, chance(80), true);
      part(cls);
      answerPending(ping, false);
      part(cls);
      break;
    }
    case 4: {
      // The same receiverAnswer param written twice, on an answer under
      // noPromisePipelining or one that failed.
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      size_t before = pending.size();
      uint32_t qn;
      int kind = pick(3);
      if (kind == 0) qn = probeCall(qr, {0}, 2, n, false, true, {});
      else if (kind == 1) qn = probeCall(-1, {}, 2, n, false, true, {});
      else qn = callOnBoot(1, n);
      part(cls);
      int64_t fwd = newIncoming(before);
      if (kind == 0 && chance(40)) {
        answerPending(fwd, chance(60));
        part(cls);
      }
      for (int i = 0; i < 2; ++i) {
        if (chance(70)) probeCall(qr, {0}, 0, n + 1 + i, false, false, {{'a', qn, {0}}});
        else probeCall(-1, {}, 4, n + 1 + i, false, false, {{'a', qn, {0}}});
        part(cls);
      }
      answerPending(fwd, chance(60));
      part(cls);
      break;
    }
    case 5: {
      // A forwarded receiverAnswer param with random hints on both calls.
      uint32_t qm = probeCall(-1, {}, chance(70) ? 2 : 7, n, false, chance(40), {}, chance(30));
      part(cls);
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      size_t before = pending.size();
      uint32_t q3 = probeCall(qr, {0}, 0, n + 2, chance(20), chance(50), {{'a', qm, {0}}},
                              chance(50));
      part(cls);
      int64_t fwd = newIncoming(before);
      if (chance(60)) {
        answerPending(fwd, chance(40));
        part(cls);
      }
      if (chance(50)) {
        sendFinish(q3, chance(50), true);
        part(cls);
      }
      break;
    }
    case 6: {
      // A plain forward under noPromisePipelining returns a promise the
      // driver later resolves; the answer's Finish and Releases follow
      // in a random order.
      uint32_t qr = callOnBoot(5, n);
      part(cls);
      uint32_t qb = callOnBoot(2, n);
      part(cls);
      size_t before = pending.size();
      uint32_t q5 = probeCall(qr, {0}, chance(70) ? 0 : 2, n + 1, chance(20), true, {});
      part(cls);
      int64_t fwd = newIncoming(before);
      uint32_t pid = answerPending(fwd, false, true);
      part(cls);
      std::vector<int> acts = {0, 1, 2, 3};
      for (size_t i = acts.size(); i > 1; --i) std::swap(acts[i - 1], acts[pick(int(i))]);
      for (int a : acts) {
        if (a == 0 && pid != 0) {
          int how = pick(4);
          if (how == 0) probeResolve(pid, hostedCap());
          else if (how == 1) probeResolve(pid, {'a', qr, {0}});
          else if (how == 2 && !questions[qb].resultImports.empty())
            probeResolve(pid, {'r', questions[qb].resultImports[0], {}});
          else probeResolve(pid, promiseCap());
        } else if (a == 1 && chance(70)) {
          sendFinish(q5, chance(50), true);
        } else if (a == 2 && chance(50) && !questions[q5].resultImports.empty()) {
          uint32_t w = questions[q5].resultImports[0];
          if (imports.count(w)) sendRelease(w, imports[w].refs, true);
        } else if (a == 3 && chance(40)) {
          callOnPipeline(q5, 0, n + 2, -1);
        } else {
          continue;
        }
        part(cls);
      }
      break;
    }
  }
}

const char* kProbes[] = {"PARAM_PROMISE_MERGE_HOSTED", "PARAM_PROMISE_MERGE_PROMISE",
                         "FWD_RA_RELEASED_PARAM",      "DISEMBARGO_PLAIN_FWD",
                         "FINISH_PLAIN_FWD_NPP",       "FINISH_PLAIN_FWD_YOURSELF",
                         "FWD_SP_RESOLVE_TO_RA",       "FWD_RA_PLAIN_PENDING",
                         "DISEMBARGO_PLAIN_FWD_CAP",   "FWD_RA_PLAIN_PENDING_CALL",
                         "DISEMBARGO_PLAIN_FWD_FINISH", "FINISH_PLAIN_FWD_NPP_CAP",
                         "FWD_RA_PLAIN_PENDING_RELEASE", "YOURSELF_NPP_PLAIN_FWD",
                         "YOURSELF_NPP_PLAIN_FWD_EXC", "YOURSELF_NPP_PLAIN_FWD_EARLY",
                         "PROXY_EARLY_FINISH", "DISEMBARGO_PLAIN_FWD_NPP",
                         "FWD_SP_RESOLVE_TO_RA_PENDING_NPP", "PARAM_PROMISE_WRITTEN_MERGE",
                         "FWD_RA_PENDING_NPP", "FWD_RA_RETURNED_NPP",
                         "DISEMBARGO_PLAIN_FWD_PROMISE_CAP", "PARAM_PROMISE_RESOLVE_TO_LIVE_IMPORT",
                         "UNKNOWN_METHOD", "UNKNOWN_INTERFACE", "CALL_PA_SELF", "PEER_ABORT",
                         "DIS_SEND_PA_RETURNED", "DIS_ACCEPT_UNIMPL",
                         "DISEMBARGO_PENDING_DEFERRED", "DISEMBARGO_PENDING_FINISHED",
                         "FINISH_QUEUED_CHAIN",
                         "TPH_PARAM_REFUSED", "DIS_FAILED_ANSWER", "DIS_PA_NON_STRUCT",
                         "FRAME_512_FULL_VALID", "FRAME_4097_FULL_VALID", "FRAME_512_HDR_ONLY",
                         "FRAME_HUGE_A", "FRAME_HUGE_B", "FRAME_HUGE_C", "DIS_YOURSELF_MAKE",
                         "OPP_SELF_REEXPORT", "DIS_YOURSELF_REFLECT", "DIS_YOURSELF_FAILED",
                         "TPH_SLOW_PREMATURE", "TPH_YOURSELF_PREMATURE", "TPH_MAKE_PREMATURE", "TPH_OPP_PREMATURE",
                         "FRAME_512_SEGS", "FRAME_U32_MAX_SEGS", "FRAME_OVERSIZED",
                         "FRAME_SEG_OVERFLOW_32", "FRAME_SHORT", "SLOW_PING_EXC_TYPES"};

// Answer the oldest pending call since `before`, as probeAnswer answers
// the newest.
void probeAnswerFirst(size_t before, bool withCap, bool promise = false) {
  if (pending.size() <= before) {
    emit("D> (no pending call)");
    return;
  }
  Incoming in = pending[before];
  pending.erase(pending.begin() + before);
  if (in.yourself) sendReturnElsewhere(in.wire);
  else sendReturnResults(in.wire, in.n + 1000, withCap, promise);
  markAnswered(in.wire);
}

bool probe(const std::string& name) {
  scenarioStep = "probe";
  scenarioPart = 0;
  window.clear();
  if (!haveBoot) return true;
  const int kSelf = 4, kMake = 2, kReflect = 5, kEcho = 0, kFail = 1, kSlow = 3, kSlowMake = 7;
  if (name == "PARAM_PROMISE_MERGE_HOSTED") {
    // Two calls keep params alive; the promise of one resolves to the
    // hosted cap of the other. Both share one import until the last Finish.
    Cap h = hostedCap(), p = promiseCap();
    uint32_t q1 = probeCall(-1, {}, kSelf, 1, false, false, {h});
    uint32_t q2 = probeCall(-1, {}, kSelf, 2, false, false, {p});
    part(name);
    probeResolve(p.id, h);
    part(name);
    sendFinish(q1, true, true);
    part(name);
    sendFinish(q2, true, true);
    part(name);
  } else if (name == "PARAM_PROMISE_MERGE_PROMISE") {
    Cap pa = promiseCap(), pb = promiseCap();
    uint32_t q1 = probeCall(-1, {}, kSelf, 1, false, false, {pa});
    uint32_t q2 = probeCall(-1, {}, kSelf, 2, false, false, {pb});
    part(name);
    probeResolve(pb.id, pa);
    part(name);
    Cap h = hostedCap();
    exports[h.id].refs -= 1;
    probeResolve(pa.id, h);
    part(name);
    sendFinish(q1, true, true);
    part(name);
    sendFinish(q2, true, true);
    part(name);
  } else if (name == "FWD_RA_RELEASED_PARAM") {
    // The export a returned answer names is released, then named again
    // through receiverAnswer in a forwarded call.
    uint32_t qm = probeCall(-1, {}, kMake, 5, false, false, {});
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    if (!questions[qm].resultImports.empty()) {
      uint32_t w = questions[qm].resultImports[0];
      sendRelease(w, 1, true);
      questions[qm].resultImports.clear();
    }
    part(name);
    size_t before = pending.size();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {{'a', qm, {0}}});
    part(name);
    probeAnswer(before, false);
    sendFinish(qf, true, true);
    part(name);
  } else if (name == "DISEMBARGO_PLAIN_FWD" || name == "DISEMBARGO_PLAIN_FWD_CAP" ||
             name == "DISEMBARGO_PLAIN_FWD_FINISH") {
    // Disembargo on an answer whose call went on as a plain call and is
    // still pending there. The FINISH variant ends the call before the
    // plain call returns.
    bool cap = name != "DISEMBARGO_PLAIN_FWD";
    bool early = name == "DISEMBARGO_PLAIN_FWD_FINISH";
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kEcho, 8, true, false, {});
    part(name);
    probeDisembargo(qp, {0}, 556);
    part(name);
    if (early) {
      sendFinish(qp, true, true);
      part(name);
    }
    if (pending.size() > before) {
      Incoming in = pending.back();
      pending.pop_back();
      sendReturnResults(in.wire, in.n + 1000, cap);
      markAnswered(in.wire);
    }
    part(name);
    if (!early) {
      sendFinish(qp, true, true);
      part(name);
    }
  } else if (name == "FINISH_PLAIN_FWD_NPP" || name == "FINISH_PLAIN_FWD_YOURSELF" ||
             name == "FINISH_PLAIN_FWD_NPP_CAP") {
    // Finish for a call that went on as a plain call, before its answer.
    // The CAP variant answers the plain call with a cap.
    bool npp = name != "FINISH_PLAIN_FWD_YOURSELF";
    bool withCap = name == "FINISH_PLAIN_FWD_NPP_CAP";
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kEcho, 9, !npp, npp, {});
    part(name);
    sendFinish(qp, true, true);
    part(name);
    if (pending.size() > before) {
      Incoming in = pending.back();
      pending.pop_back();
      sendReturnResults(in.wire, in.n + 1000, withCap);
      markAnswered(in.wire);
    }
    part(name);
  } else if (name == "FWD_SP_RESOLVE_TO_RA") {
    // A forwarded param promise resolves to receiverAnswer naming the
    // peer's own export.
    uint32_t qm = probeCall(-1, {}, kMake, 5, false, false, {});
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    Cap p = promiseCap();
    size_t before = pending.size();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {p});
    part(name);
    probeResolve(p.id, {'a', qm, {0}});
    part(name);
    probeAnswer(before, false);
    sendFinish(qf, true, true);
    part(name);
  } else if (name == "FWD_RA_PLAIN_PENDING") {
    // A forwarded call names, through receiverAnswer, an answer whose
    // call went on as a plain call and is still pending.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kMake, 8, true, false, {});
    part(name);
    size_t mid = pending.size();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {{'a', qp, {0}}});
    part(name);
    probeAnswer(mid, false);
    if (pending.size() > before) {
      Incoming in = pending[before];
      pending.erase(pending.begin() + before);
      sendReturnResults(in.wire, in.n + 1000, true);
      markAnswered(in.wire);
    }
    sendFinish(qf, true, true);
    sendFinish(qp, true, true);
    part(name);
  } else if (name == "FWD_RA_PLAIN_PENDING_RELEASE") {
    // As FWD_RA_PLAIN_PENDING, with the cap the forward wrote released
    // before the plain call returns.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kMake, 8, true, false, {});
    part(name);
    size_t mid = pending.size();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {{'a', qp, {0}}});
    part(name);
    int64_t w = importWireBySym(1);
    if (w >= 0 && imports.count(uint32_t(w))) sendRelease(uint32_t(w), imports[uint32_t(w)].refs, true);
    part(name);
    probeAnswer(mid, false);
    if (pending.size() > before) {
      Incoming in = pending[before];
      pending.erase(pending.begin() + before);
      sendReturnResults(in.wire, in.n + 1000, true);
      markAnswered(in.wire);
    }
    part(name);
    sendFinish(qf, true, true);
    sendFinish(qp, true, true);
    part(name);
  } else if (name == "FWD_RA_PLAIN_PENDING_CALL") {
    // As above, and the cap the forward wrote is called while the plain
    // call is pending, then released before the reflect answer goes.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kMake, 8, true, false, {});
    part(name);
    size_t mid = pending.size();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {{'a', qp, {0}}});
    part(name);
    int64_t w = importWireBySym(1);
    int64_t qc = w >= 0 ? int64_t(probeCall(-2 - w, {}, kEcho, 11, false, false, {})) : -1;
    part(name);
    probeAnswer(mid, false);
    if (pending.size() > before) {
      Incoming in = pending[before];
      pending.erase(pending.begin() + before);
      sendReturnResults(in.wire, in.n + 1000, true);
      markAnswered(in.wire);
    }
    part(name);
    while (pending.size() > before) probeAnswer(before, false);
    part(name);
    sendFinish(qf, true, true);
    sendFinish(qp, true, true);
    if (qc >= 0) sendFinish(uint32_t(qc), true, true);
    part(name);
    if (w >= 0 && imports.count(uint32_t(w))) sendRelease(uint32_t(w), imports[uint32_t(w)].refs, true);
    part(name);
  } else if (name == "YOURSELF_NPP_PLAIN_FWD" || name == "YOURSELF_NPP_PLAIN_FWD_EXC" ||
             name == "YOURSELF_NPP_PLAIN_FWD_EARLY") {
    // A plain call for a call with both sendResultsTo.yourself and
    // noPromisePipelining: its Return, or a Finish first.
    bool early = name == "YOURSELF_NPP_PLAIN_FWD_EARLY";
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kEcho, 9, true, true, {});
    part(name);
    if (early) {
      sendFinish(qp, true, true);
      part(name);
    }
    if (pending.size() > before) {
      Incoming in = pending.back();
      pending.pop_back();
      if (name == "YOURSELF_NPP_PLAIN_FWD_EXC") sendReturnException(in.wire, "SRVEXC");
      else sendReturnResults(in.wire, in.n + 1000, false);
      markAnswered(in.wire);
    }
    part(name);
    if (!early) {
      sendFinish(qp, true, true);
      part(name);
    }
  } else if (name == "PROXY_EARLY_FINISH") {
    // A forward writes a proxy for a pending plain call; the plain call's
    // own answer is finished before it returns, then the proxy is called.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kMake, 8, true, false, {});
    part(name);
    size_t mid = pending.size();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {{'a', qp, {0}}});
    part(name);
    probeAnswer(mid, false);
    part(name);
    sendFinish(qp, true, true);
    part(name);
    probeAnswerFirst(before, true);
    part(name);
    int64_t w = importWireBySym(1);
    int64_t qc = w >= 0 ? int64_t(probeCall(-2 - w, {}, kEcho, 11, false, false, {})) : -1;
    part(name);
    while (pending.size() > before) probeAnswer(before, false);
    part(name);
    sendFinish(qf, true, true);
    if (qc >= 0) sendFinish(uint32_t(qc), true, true);
    part(name);
    if (w >= 0 && imports.count(uint32_t(w))) sendRelease(uint32_t(w), imports[uint32_t(w)].refs, true);
    part(name);
  } else if (name == "DISEMBARGO_PLAIN_FWD_NPP") {
    // Disembargo on a pending plain call made for a noPromisePipelining
    // call.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kEcho, 8, false, true, {});
    part(name);
    probeDisembargo(qp, {0}, 556);
    part(name);
    probeAnswerFirst(before, true);
    part(name);
    sendFinish(qp, true, true);
    part(name);
  } else if (name == "FWD_SP_RESOLVE_TO_RA_PENDING_NPP") {
    // A forwarded param promise resolves to receiverAnswer on a pending
    // noPromisePipelining plain call.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kMake, 8, false, true, {});
    part(name);
    Cap p = promiseCap();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {p});
    part(name);
    probeResolve(p.id, {'a', qp, {0}});
    part(name);
    while (pending.size() > before) probeAnswer(before, false);
    part(name);
    sendFinish(qf, true, true);
    sendFinish(qp, true, true);
    part(name);
  } else if (name == "PARAM_PROMISE_WRITTEN_MERGE") {
    // A forwarded (written) param promise resolves to a promise that a
    // local call's params name, which then resolves to the peer's export.
    uint32_t qm = probeCall(-1, {}, kMake, 5, false, false, {});
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    Cap pa = promiseCap(), pb = promiseCap();
    uint32_t q1 = probeCall(-1, {}, kSelf, 1, false, false, {pa});
    size_t before = pending.size();
    uint32_t q2 = probeCall(qr, {0}, kEcho, 7, false, false, {pb});
    part(name);
    probeResolve(pb.id, {'p', pa.id, {}});
    part(name);
    if (!questions[qm].resultImports.empty())
      probeResolve(pa.id, {'r', questions[qm].resultImports[0], {}});
    part(name);
    while (pending.size() > before) probeAnswer(before, false);
    part(name);
    sendFinish(q1, true, true);
    sendFinish(q2, true, true);
    part(name);
  } else if (name == "FWD_RA_PENDING_NPP" || name == "FWD_RA_RETURNED_NPP") {
    // A forwarded receiverAnswer param names a noPromisePipelining answer:
    // a plain call still pending, or a local call that has returned.
    bool returned = name == "FWD_RA_RETURNED_NPP";
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = returned ? probeCall(-1, {}, kMake, 5, false, true, {})
                           : probeCall(qr, {0}, kMake, 8, false, true, {});
    part(name);
    size_t mid = pending.size();
    uint32_t qf = probeCall(qr, {0}, kEcho, 7, false, false, {{'a', qp, {0}}});
    part(name);
    probeAnswer(mid, false);
    part(name);
    while (pending.size() > before) probeAnswer(before, true);
    part(name);
    sendFinish(qf, true, true);
    sendFinish(qp, true, true);
    part(name);
  } else if (name == "DISEMBARGO_PLAIN_FWD_PROMISE_CAP") {
    // Disembargo on a pending plain call that returns a promise, which
    // resolves later.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t qp = probeCall(qr, {0}, kEcho, 8, true, false, {});
    part(name);
    probeDisembargo(qp, {0}, 556);
    part(name);
    uint32_t pid = nextExportId;
    probeAnswerFirst(before, true, true);
    part(name);
    Cap h = hostedCap();
    exports[h.id].refs -= 1;
    probeResolve(pid, h);
    part(name);
    sendFinish(qp, true, true);
    part(name);
  } else if (name == "PARAM_PROMISE_RESOLVE_TO_LIVE_IMPORT") {
    // A param promise resolves to a cap the peer already imports as the
    // result of a noPromisePipelining plain call. One import counts both.
    uint32_t qr = probeCall(-1, {}, kReflect, 6, false, false, {});
    part(name);
    size_t before = pending.size();
    uint32_t q1 = probeCall(qr, {0}, kMake, 8, false, true, {});
    part(name);
    uint32_t hid = nextExportId;
    probeAnswerFirst(before, true);
    part(name);
    Cap p = promiseCap();
    uint32_t q2 = probeCall(-1, {}, kSelf, 2, false, false, {p});
    part(name);
    probeResolve(p.id, {'h', hid, {}});
    part(name);
    sendFinish(q1, true, true);
    part(name);
    sendFinish(q2, true, true);
    part(name);
  } else if (name == "UNKNOWN_METHOD") {
    uint32_t q = newQuestion("unknown_method");
    capnp::MallocMessageBuilder mb;
    auto c = mb.initRoot<rpc::Message>().initCall();
    c.setQuestionId(q);
    c.setInterfaceId(TARGET_ID);
    c.setMethodId(99);
    c.initTarget().setImportedCap(bootImportWire);
    c.initParams().getContent().initAs<N>().setN(1);
    sendRaw(mb);
    emit("D> CALL q" + std::to_string(q) + " " + importRef(bootImportWire) + " Target.99");
    part(name);
    sendFinish(q, true, true);
    part(name);
  } else if (name == "UNKNOWN_INTERFACE") {
    uint64_t ids[2] = {0x123456789abcdef0ull, 0x9000000000000001ull};
    for (int i = 0; i < 2; ++i) {
      uint32_t q = newQuestion("unknown_iface");
      capnp::MallocMessageBuilder mb;
      auto c = mb.initRoot<rpc::Message>().initCall();
      c.setQuestionId(q);
      c.setInterfaceId(ids[i]);
      c.setMethodId(0);
      c.initTarget().setImportedCap(bootImportWire);
      c.initParams().getContent().initAs<N>().setN(i);
      sendRaw(mb);
      emit("D> CALL q" + std::to_string(q) + " " + importRef(bootImportWire) + " iface(" +
           std::to_string(ids[i]) + ").0");
      part(name);
      sendFinish(q, true, true);
      part(name);
    }
  } else if (name == "CALL_PA_SELF") {
    uint32_t q = newQuestion("self_pipe");
    capnp::MallocMessageBuilder mb;
    auto c = mb.initRoot<rpc::Message>().initCall();
    c.setQuestionId(q);
    c.setInterfaceId(TARGET_ID);
    c.setMethodId(0);
    auto pa = c.initTarget().initPromisedAnswer();
    pa.setQuestionId(q);
    setOps(pa, {0});
    c.initParams().getContent().initAs<N>().setN(7);
    sendRaw(mb);
    emit("D> CALL q" + std::to_string(q) + " pa(q" + std::to_string(q) + ",0) echo");
    part(name);
    sendFinish(q, true, true);
    part(name);
  } else if (name == "PEER_ABORT") {
    capnp::MallocMessageBuilder mb;
    auto ex = mb.initRoot<rpc::Message>().initAbort();
    ex.setType(rpc::Exception::Type::FAILED);
    ex.setReason("bye");
    sendRaw(mb);
    emit("D> ABORT " + exceptionStr(ex.asReader()));
    driverAborted = true;
    partAbort(name);
  } else if (name == "DIS_SEND_PA_RETURNED") {
    uint32_t q1 = callOnBoot(kMake, 1);
    part(name);
    probeDisembargo(q1, {}, 9);
    partAbort(name);
  } else if (name == "DIS_ACCEPT_UNIMPL") {
    capnp::MallocMessageBuilder mb;
    auto d = mb.initRoot<rpc::Message>().initDisembargo();
    d.initTarget().setImportedCap(bootImportWire);
    d.getContext().setAccept();
    sendRaw(mb);
    emit("D> DIS " + importRef(bootImportWire) + " accept");
    partAbort(name);
  } else if (name == "DISEMBARGO_PENDING_DEFERRED") {
    // Disembargo on an answer that still waits on the driver's Callback.
    // C++ holds it until the answer returns and keeps serving the
    // connection.
    size_t before = pending.size();
    uint32_t q1 = probeCall(-1, {}, kSlow, 1, false, false, {});
    part(name);
    probeDisembargo(q1, {0}, 5000);
    part(name);
    uint32_t q2 = probeCall(-1, {}, kEcho, 42, false, false, {});
    part(name);
    probeAnswer(before, false);
    part(name);
    sendFinish(q1, true, true);
    sendFinish(q2, true, true);
    part(name);
  } else if (name == "DISEMBARGO_PENDING_FINISHED") {
    // The Finish arrives while the Disembargo still waits.
    size_t before = pending.size();
    uint32_t q1 = probeCall(-1, {}, kSlow, 1, false, false, {});
    part(name);
    probeDisembargo(q1, {0}, 5001);
    part(name);
    sendFinish(q1, true, true);
    part(name);
    probeAnswer(before, false);
    partAbort(name);
  } else if (name == "FINISH_QUEUED_CHAIN") {
    // Finish for a queued call that has a call pipelined on it. The
    // queued call is answered as canceled, and the call behind it runs.
    size_t before = pending.size();
    uint32_t q1 = probeCall(-1, {}, kSlowMake, 1, false, false, {});
    part(name);
    uint32_t q2 = probeCall(q1, {0}, kSelf, 2, false, false, {});
    uint32_t q3 = probeCall(q2, {0}, kEcho, 3, false, false, {});
    part(name);
    sendFinish(q2, false, true);
    part(name);
    if (pending.size() > before) {
      Incoming in = pending[before];
      pending.erase(pending.begin() + before);
      sendReturnResults(in.wire, 10, false);
      markAnswered(in.wire);
    }
    part(name);
    sendFinish(q1, true, true);
    sendFinish(q3, true, true);
    part(name);
  } else if (name == "TPH_PARAM_REFUSED") {
    // Call params with thirdPartyHosted descriptors the peer refuses.
    // C++ still imports each vine and releases it with the params.
    exports[200].refs += 1;
    exports[201].refs += 1;
    uint32_t q1 = probeCall(-1, {}, kEcho, 1, false, false,
                            {{'t', 200, {}}, {'T', 201, {}}});
    part(name);
    sendFinish(q1, true, true);
    part(name);
  } else if (name == "DIS_FAILED_ANSWER") {
    // Disembargo on an answer that failed aborts with its exception.
    uint32_t q1 = probeCall(-1, {}, kFail, 1, false, false, {});
    part(name);
    probeDisembargo(q1, {0}, 77);
    partAbort(name);
  } else if (name == "DIS_PA_NON_STRUCT") {
    // A transform that walks through a capability pointer.
    uint32_t q1 = probeCall(-1, {}, kMake, 5, false, false, {});
    part(name);
    probeDisembargo(q1, {0, 0}, 78);
    partAbort(name);
  } else if (name == "FRAME_512_SEGS" || name == "FRAME_U32_MAX_SEGS" ||
             name == "FRAME_OVERSIZED" || name == "FRAME_SEG_OVERFLOW_32" ||
             name == "FRAME_SHORT") {
    // A frame header C++ refuses, or a stream that ends inside a frame.
    // The peer reports it with an Abort before it closes.
    std::vector<uint8_t> b;
    auto u32 = [&](uint32_t v) {
      for (int i = 0; i < 4; ++i) b.push_back(uint8_t(v >> (8 * i)));
    };
    if (name == "FRAME_512_SEGS") {
      u32(511);
      u32(1);
    } else if (name == "FRAME_U32_MAX_SEGS") {
      u32(0xffffffffu);
      u32(0);
    } else if (name == "FRAME_OVERSIZED") {
      u32(0);
      u32(8u * 1024 * 1024 + 1);
    } else if (name == "FRAME_SEG_OVERFLOW_32") {
      u32(1);
      u32(0x80000000u);
      u32(0x80000000u);
      u32(0);
    } else {
      u32(0);
      u32(4);
      u32(0);
      u32(0);
    }
    sendBytes(b);
    emit("D> FRAME " + name);
    if (name == "FRAME_SHORT" || name == "FRAME_512_SEGS") ::shutdown(fd, SHUT_WR);
    driverAborted = true;
    partAbort(name);
  } else if (name == "FRAME_512_FULL_VALID" || name == "FRAME_4097_FULL_VALID" ||
             name == "FRAME_512_HDR_ONLY") {
    // A well-formed frame with many segments. C++ reads every size, so
    // 512 or 4097 segments are served, and a header with no body is a
    // stream that ended inside a frame.
    uint32_t segs = name == "FRAME_4097_FULL_VALID" ? 4097 : 512;
    uint32_t q = newQuestion(kMethods[kEcho]);
    capnp::MallocMessageBuilder mb;
    auto c = mb.initRoot<rpc::Message>().initCall();
    c.setQuestionId(q);
    c.setInterfaceId(TARGET_ID);
    c.setMethodId(kEcho);
    c.initTarget().setImportedCap(bootImportWire);
    c.initParams().getContent().initAs<N>().setN(7);
    auto seg0 = mb.getSegmentsForOutput()[0].asBytes();
    std::vector<uint8_t> b;
    auto u32 = [&](uint32_t v) {
      for (int i = 0; i < 4; ++i) b.push_back(uint8_t(v >> (8 * i)));
    };
    u32(segs - 1);
    u32(uint32_t(seg0.size() / 8));
    for (uint32_t i = 1; i < segs; ++i) u32(0);
    while (b.size() % 8) b.push_back(0);
    if (name != "FRAME_512_HDR_ONLY") b.insert(b.end(), seg0.begin(), seg0.end());
    sendBytes(b);
    emit("D> FRAME " + name);
    if (name == "FRAME_512_HDR_ONLY") {
      ::shutdown(fd, SHUT_WR);
      driverAborted = true;
      partAbort(name);
    } else {
      part(name);
      sendFinish(q, true, true);
      part(name);
    }
  } else if (name == "FRAME_HUGE_A" || name == "FRAME_HUGE_B" || name == "FRAME_HUGE_C") {
    std::vector<uint8_t> b;
    auto u32 = [&](uint32_t v) {
      for (int i = 0; i < 4; ++i) b.push_back(uint8_t(v >> (8 * i)));
    };
    u32(name == "FRAME_HUGE_A" ? 100000u : name == "FRAME_HUGE_B" ? 0x7fffffffu : 0x00ffffffu);
    u32(0);
    sendBytes(b);
    emit("D> FRAME " + name);
    ::shutdown(fd, SHUT_WR);
    driverAborted = true;
    partAbort(name);
  } else if (name == "DIS_YOURSELF_MAKE") {
    uint32_t q1 = probeCall(-1, {}, kMake, 1, true, false, {});
    part(name);
    probeDisembargo(q1, {0}, 5002);
    partAbort(name);
  } else if (name == "OPP_SELF_REEXPORT") {
    // A pipelined call on an onlyPromisePipeline answer returns the
    // answer's server as a new export. The answer is finished, then
    // the export is called.
    uint32_t q1 = probeCall(-1, {}, kMake, 10, false, false, {}, true);
    part(name);
    uint32_t q2 = probeCall(q1, {0}, kSelf, 0, false, false, {});
    part(name);
    sendFinish(q1, true, true);
    part(name);
    int64_t w = questions[q2].resultImports.empty() ? -1 : int64_t(questions[q2].resultImports[0]);
    uint32_t q3 = probeCall(w >= 0 ? -2 - w : -1, {}, kEcho, 42, false, false, {});
    part(name);
    sendFinish(q3, true, true);
    sendFinish(q2, true, true);
    part(name);
  } else if (name == "DIS_YOURSELF_REFLECT") {
    // Disembargo on a local sendResultsTo.yourself answer that returns
    // the driver's own capability. C++ echoes it as receiverLoopback.
    uint32_t q1 = probeCall(-1, {}, kReflect, 1, true, false, {});
    part(name);
    probeDisembargo(q1, {0}, 5000);
    part(name);
    sendFinish(q1, true, true);
    part(name);
  } else if (name == "DIS_YOURSELF_FAILED") {
    uint32_t q1 = probeCall(-1, {}, kFail, 1, true, false, {});
    part(name);
    probeDisembargo(q1, {0}, 5001);
    partAbort(name);
  } else if (name == "TPH_SLOW_PREMATURE" || name == "TPH_YOURSELF_PREMATURE" ||
             name == "TPH_MAKE_PREMATURE" || name == "TPH_OPP_PREMATURE") {
    // A refused thirdPartyHosted param stays imported while the call
    // runs, and is released when the call finishes.
    exports[200].refs += 1;
    size_t before = pending.size();
    bool slow = name == "TPH_SLOW_PREMATURE" || name == "TPH_YOURSELF_PREMATURE";
    bool yourself = name == "TPH_YOURSELF_PREMATURE";
    bool opp = name == "TPH_OPP_PREMATURE";
    uint32_t q1 = probeCall(-1, {}, slow ? kSlow : kMake, 1, yourself, false, {{'t', 200, {}}}, opp);
    part(name);
    probeAnswer(before, false);
    part(name);
    sendFinish(q1, true, true);
    part(name);
  } else if (name == "SLOW_PING_EXC_TYPES") {
    // The ping of a slow call fails with each exception type, then
    // returns. The slow call fails alike, and the Finish for the ping
    // asks for no result release when nothing came back to release.
    using T = rpc::Exception::Type;
    struct Case {
      T type;
      const char* label;
    } cases[] = {{T::OVERLOADED, "overloaded"},
                 {T::DISCONNECTED, "disconnected"},
                 {T::UNIMPLEMENTED, "unimplemented"},
                 {T::FAILED, "failed"}};
    std::vector<uint32_t> qs;
    for (int i = 0; i < 4; ++i) {
      size_t before = pending.size();
      qs.push_back(probeCall(-1, {}, kSlow, i + 1, false, false, {}));
      part(name);
      if (pending.size() > before) {
        Incoming in = pending.back();
        pending.pop_back();
        sendReturnExceptionType(in.wire, "PINGEXC", cases[i].type, cases[i].label);
        markAnswered(in.wire);
      }
      part(name);
    }
    size_t before = pending.size();
    qs.push_back(probeCall(-1, {}, kSlow, 5, false, false, {}));
    part(name);
    probeAnswer(before, false);
    part(name);
    for (auto q : qs) sendFinish(q, true, true);
    part(name);
  } else {
    return false;
  }
  return true;
}

void step(int k) {
  std::string label;
  if (forceClass >= 0 && chance(40)) {
    forcedScenario(k, forceClass);
    return;
  }
  if (mixForced && chance(12)) {
    forcedScenario(k, pick(kNumForced));
    return;
  }
  // call, finish, release, answer, poke70, resolve, disembargo, bootstrap,
  // unknown_method, self_pipeline, abort, then one weight per scenario
  int w[16] = {35, 12, 8, pending.empty() ? 0 : 22, 2, 6, 1, 2, 3, 3, 1, 2, 2, 2, 2, 2};
  int total = 0;
  for (int x : w) total += x;
  int r = pick(total), a = 0;
  for (; a < 15; ++a) {
    if (r < w[a]) break;
    r -= w[a];
  }
  if (a >= 11) {
    scenario(k, a - 11);
    return;
  }
  window.clear();
  switch (a) {
    case 0: actCall(label); break;
    case 1: actFinish(label); break;
    case 2: actRelease(label); break;
    case 3: actAnswer(label); break;
    case 4: actPoke(label); break;
    case 5: actResolve(label); break;
    case 6: actDisembargo(label); break;
    case 7: actBoot(label); break;
    case 8: actUnknownMethod(label); break;
    case 9: actSelfPipeline(label); break;
    default: actAbort(label); break;
  }
  std::vector<std::string> acted = window;
  window.clear();
  printf("== %d %s\n", k, label.c_str());
  for (auto& s : acted) printf("%s\n", s.c_str());
  if (driverAborted) {
    waitPeerClose();
    flushWindow();
    return;
  }
  sync();
  flushWindow();
}

void endPhase() {
  printf("== end answer\n");
  for (int round = 0; round < 40 && !peerEof && !peerAborted; ++round) {
    bool waiting = false;
    for (auto& q : questions)
      if (!q.returned && !q.sync) waiting = true;
    if (pending.empty() && !waiting) break;
    while (!pending.empty()) {
      std::string l;
      actAnswer(l);
    }
    sync();
    flushWindow();
  }
  if (peerEof || peerAborted) return;
  printf("== end finish\n");
  for (auto& q : questions)
    if (!q.finished) sendFinish(q.id, false, true);
  sync();
  flushWindow();
  printf("== end release\n");
  for (auto w : liveImports(true)) {
    if (w == bootImportWire) continue;
    sendRelease(w, imports[w].refs, true);
  }
  sync();
  flushWindow();
  printf("== end boot\n");
  if (haveBoot && imports.count(bootImportWire)) sendRelease(bootImportWire, imports[bootImportWire].refs, true);
  drainIdle(200);
  flushWindow();
  printf("== end exports\n");
  for (auto& kv : exports)
    if (kv.second.refs != 0) printf("D> LEFT D%u refs=%d\n", kv.first, kv.second.refs);
}

}  // namespace

int main(int argc, char** argv) {
  if (argc < 3) {
    fprintf(stderr, "usage: %s <port> <seed> [steps [force=<CLASS>|mix]] | <port> probe:<NAME>\n", argv[0]);
    return 2;
  }
  int port = atoi(argv[1]);
  std::string probeName;
  if (strncmp(argv[2], "probe:", 6) == 0) probeName = argv[2] + 6;
  uint64_t seed = probeName.empty() ? strtoull(argv[2], nullptr, 10) : 0;
  rng.seed(seed * 0x9E3779B97F4A7C15ull + 1);
  int steps = argc > 3 ? atoi(argv[3]) : 0;
  if (steps <= 0) steps = 20 + int(rng() % 41);
  for (int i = 4; i < argc; ++i) {
    if (strcmp(argv[i], "mix") == 0) mixForced = true;
    if (strncmp(argv[i], "force=", 6) == 0) {
      for (int j = 0; j < kNumForced; ++j)
        if (strcmp(argv[i] + 6, kForced[j]) == 0) forceClass = j;
      if (forceClass < 0) {
        fprintf(stderr, "driver: unknown class %s\n", argv[i] + 6);
        return 2;
      }
    }
  }

  kj::EventLoop loop;
  kj::WaitScope ws(loop);

  for (int i = 0; i < 200; ++i) {
    fd = ::socket(AF_INET, SOCK_STREAM, 0);
    sockaddr_in sa{};
    sa.sin_family = AF_INET;
    sa.sin_port = htons(port);
    inet_pton(AF_INET, "127.0.0.1", &sa.sin_addr);
    if (::connect(fd, reinterpret_cast<sockaddr*>(&sa), sizeof(sa)) == 0) break;
    ::close(fd);
    fd = -1;
    std::this_thread::sleep_for(std::chrono::milliseconds(25));
  }
  if (fd < 0) {
    fprintf(stderr, "driver: cannot connect\n");
    return 3;
  }
  int one = 1;
  setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));

  printf("== 0 bootstrap\n");
  {
    uint32_t q = newQuestion("boot0");
    capnp::MallocMessageBuilder mb;
    mb.initRoot<rpc::Message>().initBootstrap().setQuestionId(q);
    sendRaw(mb);
    emit("D> BOOT q0");
    InMsg m;
    auto deadline = std::chrono::steady_clock::now() + std::chrono::milliseconds(kSyncTimeoutMs);
    while (!peerEof && !questions[q].returned &&
           std::chrono::steady_clock::now() < deadline) {
      if (recvOne(m, 100)) handle(m);
    }
    if (!questions[q].resultImports.empty()) {
      bootImportWire = questions[q].resultImports[0];
      haveBoot = true;
    }
    sendFinish(q, false, true);
    drainIdle(kIdleMs * 2);
    flushWindow();
  }
  if (!probeName.empty()) {
    if (!probe(probeName)) {
      fprintf(stderr, "driver: unknown probe %s\n", probeName.c_str());
      return 2;
    }
    steps = 0;
  }
  for (int k = 1; k <= steps && !peerEof && !peerAborted && !driverAborted && syncTimeouts < 3; ++k) step(k);
  if (!peerEof && !peerAborted && !driverAborted && syncTimeouts < 3) endPhase();
  if (peerAborted || peerEof) {
    drainIdle(200);
    flushWindow();
  }
  printf("== close\n");
  ::shutdown(fd, SHUT_WR);
  drainIdle(500);
  flushWindow();
  if (peerEof) printf("P> EOF\n");
  ::close(fd);
  return 0;
}
