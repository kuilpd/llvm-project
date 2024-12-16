#include "fuzzer/libfuzzer_common.h"

#include <cassert>
#include <cstdint>
#include <memory>
#include <sstream>

#include "../runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "fuzzer/ast.h"
#include "fuzzer/expr_gen.h"
#include "fuzzer/fixed_rng.h"
#include "fuzzer/gen_node.h"
#include "fuzzer/symbol_table.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Path.h"

namespace fuzzer {


namespace {

class GenNodePicker : public GenTreeVisitor {
 public:
  void visit_node(std::shared_ptr<GenNode> node) {
    if (node->is_valid()) {
      options_.emplace_back(node);
    }
  }

  template <class Rng>
  std::shared_ptr<GenNode> pick(Rng& rng) {
    std::uniform_int_distribution<size_t> distr(0, options_.size() - 1);
    return options_[distr(rng)];
  }

 private:
  std::vector<std::shared_ptr<GenNode>> options_;
};

class GenNodeWriter : public GenTreeVisitor {
 public:
  explicit GenNodeWriter(ByteWriter& writer) : writer_(writer) {}

  void visit_byte(uint8_t byte) { writer_.write_byte(byte); }

 private:
  ByteWriter& writer_;
};

ExprGenerator create_generator(SymbolTable symtab,
                               std::unique_ptr<GeneratorRng> rng) {
  auto cfg = GenConfig();
  cfg.max_depth = 12;

  return ExprGenerator(std::move(rng), cfg, symtab);
}

template <class Rng>
std::shared_ptr<GenNode> pick_random_node(std::shared_ptr<GenNode> root,
                                          Rng& rng) {
  GenNodePicker picker;
  walk_gen_tree(root, &picker);
  return picker.pick(rng);
}

void write_node(std::shared_ptr<GenNode> root, ByteWriter& writer) {
  GenNodeWriter node_writer(writer);
  walk_gen_tree(root, &node_writer);
}

}  // namespace

int LibfuzzerState::init(int* /*argc*/, char*** argv) {
  lldb::SBDebugger::Initialize();

  llvm::SmallString<256> current(__FILE__);
  llvm::sys::path::remove_filename(current);
  auto parent = llvm::sys::path::parent_path(current.str());
  auto binary_path = std::string((parent + "/Inputs/fuzzer_binary.bin").str());
  auto source_path = std::string((parent + "/Inputs/fuzzer_binary.cc").str());

  debugger_ = lldb::SBDebugger::Create(false);

  lldb::SBProcess process = LaunchTestProgram(
      debugger_, source_path.c_str(), binary_path.c_str(), "// BREAK HERE");

  target_ = process.GetTarget();
  frame_ = process.GetSelectedThread().GetSelectedFrame();

  symtab_ = fuzzer::SymbolTable::create_from_frame(
      frame_, /*ignore_qualified_types*/ true);

  // Add lldb-eval functions.
  symtab_.add_function(ScalarType::UnsignedInt, "__log2",
                       {ScalarType::UnsignedInt});

  return 0;
}

size_t LibfuzzerState::custom_mutate(uint8_t* data, size_t size,
                                     size_t max_size, unsigned int seed) {
  auto fixed_rng = std::make_unique<FixedGeneratorRng>(data, size);
  auto fixed_generator = create_generator(symtab_, std::move(fixed_rng));

  auto maybe_expr = fixed_generator.generate();
  assert(maybe_expr.has_value() && "Expression could not be generated!");

  std::mt19937 rng(seed);
  auto root = fixed_generator.node();
  auto mutable_node = pick_random_node(root, rng);

  auto random_generator =
      create_generator(symtab_, std::make_unique<DefaultGeneratorRng>(rng()));
  if (!random_generator.mutate_gen_node(mutable_node)) {
    return size;
  }

  ByteWriter writer(data, max_size);
  write_node(root, writer);

  // It's possible that `root`'s sequence of random values overflows the size of
  // `data`. Overflowed values will be ignored. This isn't ideal, but also isn't
  // critical since expression generator is able to generate an expression from
  // any byte sequence.
  // TODO: Compare number of random values in the `root` to the `max_size`.

  return writer.size();
}

std::string LibfuzzerState::input_to_expr(const uint8_t* data, size_t size) {
  auto rng = std::make_unique<FixedGeneratorRng>(data, size);
  auto generator = create_generator(symtab_, std::move(rng));
  auto maybe_expr = generator.generate();

  assert(maybe_expr.has_value() && "Expression could not be generated!");

  std::ostringstream os;
  os << maybe_expr.value();
  return os.str();
}

}  // namespace fuzzer
