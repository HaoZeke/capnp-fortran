// Load a schema the way `capnp convert` does and write a CodeGeneratorRequest.
// Unused declarations are left uncompiled, so a missing embed or a broken
// import in some other declaration does not reject the requested type.
#include <capnp/compiler/compiler.h>
#include <capnp/compiler/error-reporter.h>
#include <capnp/compiler/module-loader.h>
#include <capnp/message.h>
#include <capnp/schema.capnp.h>
#include <capnp/serialize.h>
#include <kj/debug.h>
#include <kj/filesystem.h>
#include <kj/io.h>
#include <kj/string.h>

#include <kj/map.h>

#include <cstdio>
#include <unistd.h>

#include <vector>

#define PASTE(a, b) a##b
using TextView = kj::PASTE(String, Ptr);

namespace {

class Reporter final: public capnp::compiler::GlobalErrorReporter {
public:
  void addError(const kj::ReadableDirectory& directory, kj::PathPtr path,
                SourcePos start, SourcePos end, TextView message) override {
    had = true;
    kj::String native = path.toNativeString(true);
    fprintf(stderr, "%s:%u:%u: error: %s\n", native.cStr(), start.line, start.column,
            message.cStr());
    (void)directory;
    (void)end;
  }

  bool hadErrors() override { return had; }

  bool had = false;
};

void usage() {
  fprintf(stderr,
          "usage: schema-request [--annotations keep|drop] [--no-standard-import]\n"
          "                      [-I dir]... <file> <type>\n");
}

kj::Maybe<const kj::ReadableDirectory&> openDir(
    kj::Filesystem& fs, TextView text,
    std::vector<kj::Own<const kj::ReadableDirectory>>& owned) {
  auto path = fs.getCurrentPath().evalNative(text);
  if (path.size() == 0) return fs.getRoot();
  KJ_IF_MAYBE(dir, fs.getRoot().tryOpenSubdir(path)) {
    auto& ref = **dir;
    owned.push_back(kj::mv(*dir));
    return ref;
  }
  return nullptr;
}

}  // namespace

int main(int argc, char** argv) {
  bool annotations = false;
  bool standardImport = true;
  std::vector<kj::String> importText;
  TextView fileText;
  TextView typeText;
  int positional = 0;

  for (int i = 1; i < argc; i++) {
    TextView arg = argv[i];
    if (arg == "--annotations") {
      if (i + 1 >= argc) {
        usage();
        return 1;
      }
      TextView mode = argv[++i];
      if (mode == "keep") annotations = true;
      else if (mode == "drop") annotations = false;
      else {
        usage();
        return 1;
      }
    } else if (arg == "--no-standard-import") {
      standardImport = false;
    } else if (arg == "-I") {
      if (i + 1 >= argc) {
        usage();
        return 1;
      }
      importText.push_back(kj::str(argv[++i]));
    } else if (arg.startsWith("-I") && arg.size() > 2) {
      importText.push_back(kj::str(arg.slice(2)));
    } else if (arg.startsWith("-")) {
      usage();
      return 1;
    } else if (positional == 0) {
      fileText = arg;
      positional = 1;
    } else if (positional == 1) {
      typeText = arg;
      positional = 2;
    } else {
      usage();
      return 1;
    }
  }
  if (positional != 2) {
    usage();
    return 1;
  }

  Reporter reporter;
  auto fs = kj::newDiskFilesystem();
  capnp::compiler::ModuleLoader loader(reporter);
  std::vector<kj::Own<const kj::ReadableDirectory>> owned;

  for (auto& text: importText) {
    KJ_IF_MAYBE(dir, openDir(*fs, text, owned)) {
      loader.addImportPath(*dir);
    } else {
      fprintf(stderr, "no such directory: %s\n", text.cStr());
      return 1;
    }
  }
  if (standardImport) {
    static const char* kStandard[] = {
      "/usr/local/include",
      "/usr/include",
    };
    // The directory that holds this capnp install's headers, when the
    // binary lives in <prefix>/bin.
    char self[4096];
    ssize_t nself = readlink("/proc/self/exe", self, sizeof(self) - 1);
    kj::String installInclude;
    if (nself > 0) {
      self[nself] = 0;
      TextView exe(self);
      KJ_IF_MAYBE(slash, exe.findLast('/')) {
        auto bin = exe.slice(0, *slash);
        KJ_IF_MAYBE(slash2, bin.findLast('/')) {
          installInclude = kj::str(bin.slice(0, *slash2), "/include");
        }
      }
    }
    const char* extra[3];
    int nextra = 0;
    extra[nextra++] = "/usr/local/include";
    extra[nextra++] = "/usr/include";
    if (installInclude.size() > 0) extra[nextra++] = installInclude.cStr();
    (void)kStandard;
    for (int i = 0; i < nextra; i++) {
      KJ_IF_MAYBE(dir, openDir(*fs, extra[i], owned)) {
        loader.addImportPath(*dir);
      }
    }
  }

  auto filePath = fs->getCurrentPath().evalNative(fileText);
  KJ_REQUIRE(filePath.size() > 0, "empty schema path");
  const kj::ReadableDirectory* base = &fs->getRoot();
  kj::Path within = filePath.clone();
  if (filePath.startsWith(fs->getCurrentPath())) {
    base = &fs->getCurrent();
    within = filePath.slice(fs->getCurrentPath().size(), filePath.size()).clone();
  }
  KJ_IF_MAYBE(module, loader.loadModule(*base, within)) {
    auto flag = annotations ? capnp::compiler::Compiler::COMPILE_ANNOTATIONS
                            : capnp::compiler::Compiler::DROP_ANNOTATIONS;
    capnp::compiler::Compiler compiler(flag);
    auto scope = compiler.add(*module);
    compiler.eagerlyCompile(scope.getId(), capnp::compiler::Compiler::NODE);
    if (reporter.hadErrors()) return 1;

    // `capnp eval` names a const or a field path, not only a type.
    // Resolve the longest prefix of nested nodes and ignore subscripts.
    // The rest of the path is a field walk done by the caller.
    uint64_t id = scope.getId();
    size_t begin = 0;
    bool resolved = false;
    TextView type = typeText;
    while (begin <= type.size()) {
      size_t dot = type.size();
      for (size_t i = begin; i < type.size(); i++) {
        if (type[i] == '.') {
          dot = i;
          break;
        }
      }
      auto raw = type.slice(begin, dot);
      size_t nlen = raw.size();
      KJ_IF_MAYBE(bracket, raw.findFirst('[')) {
        nlen = *bracket;
      }
      if (nlen == 0) {
        if (!resolved) {
          fprintf(stderr, "unknown type %s\n", type.cStr());
          return 1;
        }
        break;
      }
      // The text view requires a NUL after the last byte. The slice sits
      // in the middle of the path, so copy it.
      kj::String name = kj::str(raw.slice(0, nlen));
      KJ_IF_MAYBE(child, compiler.lookup(id, name)) {
        id = *child;
        resolved = true;
      } else if (!resolved) {
        fprintf(stderr, "unknown type %s\n", type.cStr());
        return 1;
      } else {
        break;
      }
      if (dot == type.size()) break;
      begin = dot + 1;
    }

    unsigned int eagerness = capnp::compiler::Compiler::NODE | capnp::compiler::Compiler::PARENTS |
                     capnp::compiler::Compiler::DEPENDENCIES |
                     capnp::compiler::Compiler::DEPENDENCY_DEPENDENCIES |
                     capnp::compiler::Compiler::DEPENDENCY_PARENTS;
    compiler.eagerlyCompile(id, eagerness);
    if (reporter.hadErrors()) return 1;

    capnp::MallocMessageBuilder message;
    auto request = message.initRoot<capnp::schema::CodeGeneratorRequest>();
    auto schemas = compiler.getLoader().getAllLoaded();
    auto nodes = request.initNodes(schemas.size());
    for (size_t i = 0; i < schemas.size(); i++) {
      nodes.setWithCaveats(i, schemas[i].getProto());
    }
    kj::FdOutputStream out(STDOUT_FILENO);
    capnp::writeMessage(out, message);
    return 0;
  }

  fprintf(stderr, "no such file: %s\n", fileText.cStr());
  return 1;
}
